# -*- coding: utf-8 -*-
require 'rubygems'
require 'spec'
require 'automaton'

include Automaton

module RecognizeMatcher
  class Recognize
    def initialize(expected)
      @expected = expected
    end

    def matches?(target)
      @target = target
      @target.recognize?(@expected)
    end

    def failure_message
      "expected <#{to_string(@target)}> to " +
      "recognize the input tape <#{to_string(@expected)}>"
    end

    def negative_failure_message
      "expected <#{to_string(@target)}> not to " +
      "recognize the input tape <#{to_string(@expected)}>"
    end

    # Returns string representation of an object.
    def to_string(value)
      # indicate a nil
      if value.nil?
        'nil'
      end

      # join arrays
      if value.class == Array
        return value.join(", ")
      end

      # otherwise return to_s() instead of inspect()
      return value.to_s
    end
  end

  # Actual matcher that is exposed.
  def recognize(expected)
    Recognize.new(expected)
  end
end

Spec::Runner.configure do |config|
  config.include(RecognizeMatcher)
end

describe DSL do
  it "should initialize correctly" do
    @a = DFSA.new({
                    :q0 => {'a' => :q1, 'c' => :q3},
                    :q1 => {'b' => :q2},
                    :q2 => {'c' => :q3}
                  }, :q0, [:q3])

    @b = Automaton.setup(:dfsa) do
      initial_state :q0 do
        transition 'a', :q1
        transition 'c', :q3
      end

      state :q1 do
        transition 'b', :q2
      end

      state :q2 do
        transition 'c', :q3
      end

      final_state :q3
    end

    @b.transitions.should == @a.transitions
  end
end

describe DFSA do
  describe "automaton for norwegian numbers" do
    it "should bla" do
      @num = Automaton.setup(:dfsa) do
        initial_state :t0 do
          transition :terminal_nums, :t2
          transition :tens, :t1
        end

        state :t1 do
          transition :small_nums, :t2
        end

        final_states :t1, :t2
      end

      @num.add_cat :small_nums,    %w[ein to tre fire fem seks sju åtte ni]

      @num.add_cat :terminal_nums, %w[ein to tre fire fem seks sju åtte ni
                                      ti elleve tolv tretten fjorten femten
                                      seksten sytten atten nitten]

      @num.add_cat :tens, %w[tjue tretti førti femti seksti sytti åtti nitti]

      @num.should recognize %w(ein)
      @num.should recognize %w(tretti to)
      @num.should recognize %w(tretti)

      @num.should_not recognize(%w[tretti tretti])
    end
  end

  describe "the Ruby-language" do
    before(:each) do
      @r = DFSA.new({
                      :q0 => {"R" => :q1},
                      :q1 => {"u" => :q2},
                      :q2 => {"u" => :q2, "b" => :q3},
                      :q3 => {"y" => :q4}
                    }, :q0, [:q4])
    end

    it "should recognize valid sentences" do
      @r.should recognize %w(R u b y)
      @r.should recognize %w(R u u u u b y)
    end

    it "should reject invalid sentences" do
      @r.should_not recognize %w(r u b y)
      @r.should_not recognize %w(R u b)
    end

    it "should be possible to negate the automaton" do
      @r.negate!
      @r.should recognize %w(R u b)
      @r.should_not recognize %w(R u b y)
      @r.negate! # Negating again should work as expected
      @r.should recognize %w(R u b y)
    end
  end

  describe "automaton for a uninteresting language without loops" do
    before(:each) do
      @a = DFSA.new({
                      :q0 => {'a' => :q1, 'c' => :q3},
                      :q1 => {'b' => :q2},
                      :q2 => {'c' => :q3}
                    }, :q0, [:q3])
    end

    it "should recognize valid strings" do
      @a.should recognize(%w[c])
      @a.should recognize(%w[a b c])
    end

    it "should reject invalid strings" do
      @a.should_not recognize(%[a])
      @a.should_not recognize(%w[a b c d])
    end
  end

  describe "automaton for the a*b* language" do
    before(:each) do
      @a = DFSA.new({
                      :q0 => {'a' => :q0, 'b' => :q1},
                      :q1 => {'b' => :q1}
                    }, :q0, [:q1])
    end

    it "should recognize a valid string" do
      @a.should recognize(%w[a b])
      @a.should recognize(%w[a b b b b])
      @a.should recognize(%w[a a a b b b])
    end

    it "should reject invalid strings" do
      @a.should_not recognize(%w[a a b b x])
      @a.should_not recognize(%w[a a a b a a a])
    end

    it "should derive its own alphabet" do
      @a.alphabet.should == Set.new(%w(a b))
    end
  end

  describe "deterministic automaton for the sheeptalk language" do
    before(:each) do
      @sheeptalk = DFSA.new({
                              :q0 => {'b' => :q1},
                              :q1 => {'a' => :q2},
                              :q2 => {'a' => :q3},
                              :q3 => {'a' => :q3, '!' => :q4}
                            }, :q0, [:q4])
    end

    it "should accept a valid string" do
      @sheeptalk.should recognize(%w[b a a a a a a !])
    end

    it "should derive its own alphabet" do
      @sheeptalk.alphabet.should == Set.new(%w[a b !])
    end

    it "should derive the set of states" do
      @sheeptalk.states.should == Set.new([:q0, :q1, :q2, :q3, :q4])
    end
  end
end

describe NDFSA do
  describe "Automatons for parsing of English sentences" do
    it "should pare some sentences" do
      @lexicon = {
        :det => %w(the a),
        :adj => %w(large expensive),
        :n => %w(man car),
        :v => %w(started walked stopped),
        :adv => %w(quickly slowly)
      }

      @np = NDFSA.new({
                        :np0 => { :det => [:np1, :np2] },
                        :np1 => { :adj => :np2 },
                        :np2 => { :n => :np3 },
                      }, :np0, [:np3], @lexicon)

      @np.should recognize %w(the large man)
      @np.should recognize %w(the expensive car)
      @np.should recognize %w(the man)
      @np.should_not recognize %w(large man)

      @vp = NDFSA.new({
                        :vp0 => { :v => :vp1 },
                        :vp1 => { :adv => :vp2 }
                      }, :vp0, [:vp1, :vp2], @lexicon)

      @vp.should recognize %w(walked)
      @vp.should recognize %w(walked slowly)

      @s = @np + @vp
      @s.should recognize %w(the man walked slowly)
    end
  end

  describe "Sheeptalk NDFSA, with epsilon" do
    before(:each) do
      @sheeptalk = NDFSA.new({
                              :q0 => {'b' => :q1},
                              :q1 => {'a' => :q2},
                              :q2 => {'a' => :q3},
                              :q3 => {nil => :q2, '!' => :q4}
                            }, :q0, [:q4])
    end

    it "should accept a valid string" do
      @sheeptalk.should recognize(%w[b a a a a a a !])
      @sheeptalk.should recognize(%w[b a a !])
    end

    it "should reject a invalid string" do
      @sheeptalk.should_not recognize(%w[b a !])
    end
  end

  describe "Sheeptalk NDFSA, with early loop" do
    before(:each) do
      @sheeptalk = NDFSA.new({
                               :q0 => {'b' => :q1},
                               :q1 => {'a' => :q2},
                               :q2 => {'a' => [:q2, :q3]},
                               :q3 => {'!' => :q4}
                             }, :q0, [:q4])
    end

    it "should accept a valid string" do
      @sheeptalk.should recognize %w(b a a a a a a a a a !)
    end
  end

  describe "An example language" do
    it "should accept and reject strings" do
      @a = Automaton.setup(:ndfsa) do
        initial_state :q0 do
          transition 'en', :q1
          transition 'den', :q2
        end

        state :q1 do
          transition 'gammel', :q1
          transition 'mann', :q3
        end

        state :q2 do
          transition 'gamle', :q2
          transition 'mannen', :q3
        end

        state :q3 do
          transition 'sover', :q4
        end

        final_state :q4
      end

      @a.should recognize(%w(en gammel mann sover))
      @a.should recognize(%w(en gammel gammel gammel mann sover))
      @a.should recognize(%w(den gamle mannen sover))
      @a.should recognize(%w(den gamle gamle mannen sover))

      @a.should_not recognize(%w(den gamle gamle mannen))
      @a.should_not recognize(%w(mannen sover))
    end
  end
end

describe "Category FSA" do
  it "should be possible to define a simple category-based language" do
    @vb = DFSA.new({
                     :q0 => {:vowel => :q1},
                     :q1 => {'b' => :q2}
                   }, :q0, [:q2])

    @vb.add_cat :vowel, %w[a e i o u y]
    @vb.should recognize(%w[e b])
    @vb.should_not recognize(%w[x b])

    @vb.alphabet.should == Set.new(%w[a e i o u y b])
  end
end


describe "Pushdown Automaton" do
  it "should be possible to create a PDA that accepts the a^n b^n language" do
    @anbn = Automaton.setup(:pda) do
      initial_state :q0 do
        transition ['a', EMPTY], [:q0, 'A']
        transition ['b', 'A'],   [:q1, EMPTY]
      end

      state :q1 do
        transition ['b', 'A'],   [:q1, EMPTY]
      end

      final_states :q0, :q1
    end

    @anbn.should recognize(%w()) # empty input, e
    @anbn.should recognize(%w(a a b b))

    @anbn.should_not recognize(%w(b a))
    @anbn.should_not recognize(%w(a a a b b))
    @anbn.should_not recognize(%w(a a b b b))
    @anbn.should_not recognize(%w(a a b b a))
    @anbn.should_not recognize(%w(a b a b))
  end

  it "should be possible to create a PDA that accepts the mirror language" do
    @ml = Automaton.setup(:pda) do
      initial_state :q0 do
        transition ['a', EMPTY], [:q0, 'A']
        transition ['a', 'A'],   [:q1, EMPTY]
        transition ['b', EMPTY], [:q0, 'B']
        transition ['b', 'B'],   [:q1, EMPTY]
      end

      state :q1 do
        transition ['a', 'A'],   [:q1, EMPTY]
        transition ['b', 'B'],   [:q1, EMPTY]
      end

      final_states :q0, :q1
    end

    @ml.should recognize(%w(b a a b))
    @ml.should recognize(%w(a b b a))
    @ml.should recognize(%w(b a a b b a a b))
    @ml.should recognize(%w(b b b b b b b b))

    @ml.should_not recognize(%w(b a a a b a))
    @ml.should_not recognize(%w(b a a b b a a a))
    @ml.should_not recognize(%w(b b a))
  end
end
