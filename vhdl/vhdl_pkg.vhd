library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

package vhdl_pkg is
    -- Ternary operator for easier porting:
    -- predicate ? true_value : false_value will be translated to ternary(predicate, true_value, false_value)
    function ternary(predicate   : boolean; t : integer;f : integer) return integer;
    function ternary(check_value : integer; t : integer;f : integer) return integer;
    function ternary(predicate   : boolean; t : std_logic;f : std_logic) return std_logic;
    function ternary(check_value : integer; t : std_logic;f : std_logic) return std_logic;
    function ternary(predicate   : boolean; t : std_logic_vector;f : std_logic_vector) return std_logic_vector;
    function ternary(check_value : integer; t : std_logic_vector;f : std_logic_vector) return std_logic_vector;

    -- Ceil log2 of value: How many bits do I need to represent the value in binary.
    function clog2(value         : integer) return integer;

    -- Create vector with size w and all bits set to '0'
    function const_0(width       : integer) return std_logic_vector;

    -- Create vector with size w and all bits set to '1'
    function const_1(width       : integer) return std_logic_vector;

    -- Create vector with size w and all bits set to v
    function const_V(width       : integer; v : std_logic) return std_logic_vector;

    -- mask data and check result for equality: (data & mask) == check
    function mask_check(data     : std_logic_vector; mask : std_logic_vector; check : std_logic_vector) return boolean;

    -- conditionally invert data and check result for equality: (data ^ mask) == check
    function xor_check(data      : std_logic_vector; mask : std_logic_vector; check : std_logic_vector) return boolean;

    -- common fifo is full check of two pointers: full when first MSB different but rest same
    function fifo_is_full(a      : unsigned; b : unsigned) return boolean;
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

    function const_V(width : integer; v : std_logic) return std_logic_vector is
        variable const         : std_logic_vector(width - 1 downto 0) := (others => v);
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

    function xor_check(data : std_logic_vector; mask : std_logic_vector; check : std_logic_vector) return boolean is
        constant common_len     : natural                           := maximum(data'length, maximum(mask'length, check'length));
        variable data_u         : unsigned(common_len - 1 downto 0) := resize(unsigned(data), common_len);
        variable mask_u         : unsigned(common_len - 1 downto 0) := resize(unsigned(mask), common_len);
        variable check_u        : unsigned(common_len - 1 downto 0) := resize(unsigned(check), common_len);
    begin
        return ((data_u xor mask_u) = check_u);
    end function;

    function fifo_is_full(a : unsigned; b : unsigned) return boolean is begin
        -- full when first MSB different but rest same
        return a(a'length - 1) /= b(b'length - 1) and a(a'length - 2 downto 0) = b(b'length - 2 downto 0);
    end function;
end package body;