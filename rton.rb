# -*- coding: utf-8 -*-

# Finite State Automatons in Ruby. Includes DFSA, NDFSA and PDAs.
# Developed for pedagogical purposes.
#
# Author: Bjørn Arild Mæland <bjorn.maeland@gmail.com>

require "set"

# Core extensions
class Hash
  def push_at(k,v)
    self[k] = [] unless self[k]
    self[k] << v
  end
end

module Automaton
  EMPTY = '' # The empty string, e

  def self.setup(type, &blk)
    a = case type
          when :dfsa  then DFSA.new
          when :ndfsa then NDFSA.new
          when :pda   then PDA.new
          else NDFSA.new # Default to NDFSA
        end
    DSL.new(a, &blk)
    a
  end

  class DSL
    def initialize(subject, &blk)
      @subject = subject
      instance_eval(&blk)
    end

    def initial_state(name, &transitions)
      @state = name
      @subject.transitions[@state] = {}
      @subject.initial_state = @state
      instance_eval(&transitions)
    end

    def state(name, &blk)
      @state = name
      @subject.transitions[@state] = {}
      instance_eval(&blk)
    end

    # Add transition to transition table
    def transition(symbol, to)
      @subject.transitions[@state][symbol] = to
    end

    # Syntactical sugar for the dsl
    def final_state(state)
      final_states(state)
    end

    # Set final states for this automaton
    def final_states(*states)
      @subject.final_states = states
    end
  end

  # A search situation
  class Situation
    attr_accessor :state, :index, :stack

    def initialize(state, index, stack = [])
      @state = state
      @index = index
      @stack = stack
    end
  end

  # Abstract base class
  class Automaton
    attr_accessor :initial_state, :final_states, :transitions, :cats, :debug

    def initialize(node_graph = {},
                   initial_state = nil,
                   final_states = [],
                   lexicon = nil)
      @transitions = node_graph
      @final_states = final_states
      @initial_state = initial_state
      @cats = lexicon || {}
      @negated = false
      @debug = false
    end

    # A set contanining the alphabet
    def alphabet
      alpha = []
      @transitions.each { |k,v| alpha += v.keys }
      Set.new(expand_symbols(alpha))
    end

    # A set containing the states
    def states
      states = Set.new([@initial_state])
      @transitions.each { |k,v| states += v.values }
      states
    end

    # Negates the automaton - the automaton will then recognize the complement
    # of the previously recognized language.
    def negate!
      @negated = @negated ? false : true
    end

    # Recognize an input tape. Returns true or false
    def recognize?(tape)
      p @transitions if debug
      @tape = tape

      # The agenda contains the list of automaton situations
      # - the automaton's "meta memory"
      agenda = [ Situation.new(@initial_state, 0) ]

      until agenda.empty? do
        current_situation = agenda.pop
        if final_situation? current_situation
          return @negated ? false : true
        end
        generate_new_situations(current_situation).each { |new| agenda.push(new) }
        p agenda if debug
      end
      @negated ? true : false
    end

    def add_cat(cat, lex)
      @cats[cat] = lex
    end

    # Main purpose of this method is to expand categories.
    # Currently does not support recursive definitions
    def transition_symbols_for(state)
      trans = {}
      return trans if @transitions[state].nil?
      @transitions[state].each do |k,v|
        tmp = v.is_a?(Array) ? v : [v]
        tmp.each do |i|
          k.is_a?(Symbol) ? @cats[k].each {|s| trans.push_at(s, v)} : trans.push_at(k, i)
        end
      end
      trans
    end

    # Determines if a situation is final
    def final_situation?(sit)
      if sit.index == @tape.length
        return true if @final_states.include? sit.state
      end
      false
    end

    # Extend this automaton with another.
    def +(other)
      transitions = @transitions.merge(other.transitions)
      # Add a new empty transition from the final states in the current
      # automaton to the inital state in the adjoined automaton.
      @final_states.each do |s|
        if transitions[s]
          transitions[s].merge!({nil => other.initial_state })
        else
          transitions[s] = {nil => other.initial_state }
        end
      end

      # Should check automaton type here instead of defaulting to NDFSA
      NDFSA.new(transitions, @initial_state, other.final_states, @cats)
    end

    private
    def expand_symbols(list)
      r = []
      list.each { |e| e.is_a?(Symbol) ? r += @cats[e] : r << e }
      r
    end
  end

  # Deterministic Finite State Automaton
  # These arent very useful, but are included to show their limitations
  class DFSA < Automaton
    def generate_new_situations(sit)
      transitions = transition_symbols_for(sit.state)
      if transitions.keys.include? @tape[sit.index]
        return [ Situation.new(transitions[@tape[sit.index]].first, sit.index + 1) ]
      end
      []
    end
  end

  # Non-deterministic Finite State Automaton
  class NDFSA < Automaton
    def generate_new_situations(sit)
      transitions = transition_symbols_for(sit.state)
      res = []
      if transitions.keys.include? @tape[sit.index]
        transitions[@tape[sit.index]].each do |ns|
          # TODO clean up this
          if ns.is_a?(Array)
            ns.each { |s| res << Situation.new(s, sit.index+1) }
          else
            res << Situation.new(ns, sit.index+1)
          end
        end
      elsif transitions.keys.include? nil # Try EPSILON
        transitions[nil].each do |ns|
          res << Situation.new(ns, sit.index)
        end
      end
      res
    end
  end

  # Pushdown Automaton
  class PDA < Automaton
    def generate_new_situations(sit)
      transitions = transition_symbols_for(sit.state)
      res = []
      transitions.each do |tr|
        next unless tr.assoc(@tape[sit.index])

        stack_match = tr[0][1]
        unless stack_match.empty? # if empty, don't read from stack
          next if stack_match != sit.stack.last
        end

        push_sym = tr[1][1]
        new_stack = sit.stack.dup # copy current situation's stack
        if push_sym.empty?
          new_stack.pop
        else
          new_stack.push(push_sym)
        end

        res.push Situation.new(tr[1][0], sit.index+1, new_stack)
      end
      res
    end

    # Also check if the stack is empty
    def final_situation?(sit)
      (super(sit) and sit.stack.empty?) ? true : false
    end
  end
end
