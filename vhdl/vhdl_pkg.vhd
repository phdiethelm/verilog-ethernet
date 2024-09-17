library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

package vhdl_pkg is
    function ternary(predicate   : boolean; t : integer;f : integer) return integer;
    function ternary(check_value : integer; t : integer;f : integer) return integer;
    function ternary(predicate   : boolean; t : std_logic;f : std_logic) return std_logic;
    function ternary(check_value : integer; t : std_logic;f : std_logic) return std_logic;
    function ternary(predicate   : boolean; t : std_logic_vector;f : std_logic_vector) return std_logic_vector;
    function ternary(check_value : integer; t : std_logic_vector;f : std_logic_vector) return std_logic_vector;
    function clog2(value         : integer) return integer;
    function const_0(width       : integer) return std_logic_vector;
    function const_1(width       : integer) return std_logic_vector;
    function mask_check(data     : std_logic_vector; mask : std_logic_vector; check : std_logic_vector) return boolean;
end package;

package body vhdl_pkg is

    function ternary(predicate : boolean;t : integer;f : integer) return integer is begin
        if predicate then
            return t;
        end if;
        return f;
    end function;

    function ternary(check_value : integer;t : integer;f : integer) return integer is begin
        return ternary(check_value /= 0, t, f);
    end function;

    function ternary(predicate : boolean; t : std_logic;f : std_logic) return std_logic is begin
        if predicate then
            return t;
        end if;
        return f;
    end function;

    function ternary(check_value : integer;t : std_logic;f : std_logic) return std_logic is begin
        return ternary(check_value /= 0, t, f);
    end function;

    function ternary(predicate : boolean; t : std_logic_vector;f : std_logic_vector) return std_logic_vector is begin
        if predicate then
            return t;
        end if;
        return f;
    end function;

    function ternary(check_value : integer;t : std_logic_vector;f : std_logic_vector) return std_logic_vector is begin
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

    function mask_check(data : std_logic_vector; mask : std_logic_vector; check : std_logic_vector) return boolean is
        constant common_len      : natural                           := maximum(data'length, maximum(mask'length, check'length));
        variable data_u          : unsigned(common_len - 1 downto 0) := resize(unsigned(data), common_len);
        variable mask_u          : unsigned(common_len - 1 downto 0) := resize(unsigned(mask), common_len);
        variable check_u         : unsigned(common_len - 1 downto 0) := resize(unsigned(check), common_len);
    begin
        return ((data_u and mask_u) = check_u);
    end function;
end package body;