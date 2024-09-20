library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

package vhdl_pkg is
    -- Ternary operator for easier porting:
    -- predicate ? true_value : false_value will be translated to ternary(predicate, true_value, false_value)
    function ternary(predicate   : boolean; t : integer; f : integer) return integer;
    function ternary(check_value : integer; t : integer; f : integer) return integer;
    function ternary(predicate   : boolean; t : std_logic; f : std_logic) return std_logic;
    function ternary(check_value : integer; t : std_logic; f : std_logic) return std_logic;
    function ternary(predicate   : boolean; t : std_logic_vector; f : std_logic_vector) return std_logic_vector;
    function ternary(check_value : integer; t : std_logic_vector; f : std_logic_vector) return std_logic_vector;
    function ternary(predicate   : boolean; t : unsigned; f : unsigned) return unsigned;
    function ternary(check_value : integer; t : unsigned; f : unsigned) return unsigned;
    function ternary(predicate   : boolean; t : boolean; f : boolean) return boolean;
    function ternary(check_value : integer; t : boolean; f : boolean) return boolean;

    -- Ceil log2 of value: How many bits do I need to represent the value in binary.
    function clog2(value         : integer) return integer;

    -- Create vector with size w and all bits set to '0'
    function const_0(width       : integer) return std_logic_vector;

    -- Create vector with size w and all bits set to '1'
    function const_1(width       : integer) return std_logic_vector;

    -- Create vector with size w and all bits set to v
    function const_V(width       : integer; v : std_logic) return std_logic_vector;
end package;

package body vhdl_pkg is

    function ternary(predicate : boolean; t : integer; f : integer) return integer is begin
        if predicate then
            return t;
        end if;
        return f;
    end function;

    function ternary(check_value : integer; t : integer; f : integer) return integer is begin
        return ternary(check_value /= 0, t, f);
    end function;

    function ternary(predicate : boolean; t : std_logic; f : std_logic) return std_logic is begin
        if predicate then
            return t;
        end if;
        return f;
    end function;

    function ternary(check_value : integer; t : std_logic; f : std_logic) return std_logic is begin
        return ternary(check_value /= 0, t, f);
    end function;

    function ternary(predicate : boolean; t : std_logic_vector; f : std_logic_vector) return std_logic_vector is begin
        if predicate then
            return t;
        end if;
        return f;
    end function;

    function ternary(check_value : integer; t : std_logic_vector; f : std_logic_vector) return std_logic_vector is begin
        return ternary(check_value /= 0, t, f);
    end function;

    function ternary(predicate : boolean; t : unsigned; f : unsigned) return unsigned is begin
        if predicate then
            return t;
        end if;
        return f;
    end function;

    function ternary(check_value : integer; t : unsigned; f : unsigned) return unsigned is begin
        return ternary(check_value /= 0, t, f);
    end function;

    function ternary(predicate : boolean; t : boolean; f : boolean) return boolean is begin
        if predicate then
            return t;
        end if;
        return f;
    end function;

    function ternary(check_value : integer; t : boolean; f : boolean) return boolean is begin
        return ternary(check_value /= 0, t, f);
    end function;

    function clog2(value : integer) return integer is begin
        return integer(ceil(log2(real(value))));
    end function;

    function const_0(width : integer) return std_logic_vector is
        variable const         : std_logic_vector(width - 1 downto 0) := (others => '0');
    begin
        return const;
    end function;

    function const_1(width : integer) return std_logic_vector is
        variable const         : std_logic_vector(width - 1 downto 0) := (others => '1');
    begin
        return const;
    end function;

    function const_V(width : integer; v : std_logic) return std_logic_vector is
        variable const         : std_logic_vector(width - 1 downto 0) := (others => v);
    begin
        return const;
    end function;
end package body;