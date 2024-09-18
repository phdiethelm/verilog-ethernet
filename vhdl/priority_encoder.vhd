-- 
-- Copyright (c) 2014-2021 Alex Forencich
-- 
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
-- 
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
-- 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.
-- 

-- Language: VHDL 2008

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.vhdl_pkg.all;

-- 
--  Priority encoder module
-- 
entity priority_encoder is

    generic (
        WIDTH             : integer := 4;
        -- LSB priority selection
        LSB_HIGH_PRIORITY : integer := 0
    );
    port (
        signal input_unencoded  : in  std_logic_vector(WIDTH - 1 downto 0);
        signal output_valid     : out std_logic;
        signal output_encoded   : out std_logic_vector(clog2(WIDTH) - 1 downto 0);
        signal output_unencoded : out std_logic_vector(WIDTH - 1 downto 0)
    );
end entity;

architecture rtl of priority_encoder is

    constant LEVELS     : integer := ternary(WIDTH > 2, clog2(WIDTH), 1);
    constant W          : integer := 2 ** LEVELS;

    -- pad input to even power of two
    signal input_padded : std_logic_vector(W - 1 downto 0);

    type t_stage is array (natural range <>) of std_logic_vector(W/2 - 1 downto 0);
    signal stage_valid : t_stage(LEVELS - 1 downto 0) := (others => (others => '0'));
    signal stage_enc   : t_stage(LEVELS - 1 downto 0) := (others => (others => '0'));

begin

    input_padded <= const_0(W - WIDTH) & input_unencoded;

    -- process input bits; generate valid bit and encoded bit for each pair
    g_loop_in : for n in 0 to W/2 - 1 generate

        stage_valid(0)(n) <= or input_padded(n * 2 + 1 downto n * 2);

        g1 : if LSB_HIGH_PRIORITY = 1 generate
            -- bit 0 is highest priority
            stage_enc(0)(n) <= not input_padded(n * 2 + 0);
        end generate;

        g0 : if LSB_HIGH_PRIORITY = 0 generate
            -- bit 0 is lowest priority
            stage_enc(0)(n) <= input_padded(n * 2 + 1);
        end generate;
    end generate;

    -- compress down to single valid bit and encoded bus
    g_loop_levels : for l in 1 to LEVELS - 1 generate
        loop_compress : for n in 0 to (W/(2 * 2 ** l)) - 1 generate
            stage_valid(l)(n) <= or stage_valid(l - 1)(n * 2 + 1 downto n * 2);

            g1 : if LSB_HIGH_PRIORITY = 1 generate
                -- bit 0 is highest priority
                stage_enc(l)((n + 1) * (l + 1) - 1 downto n * (l + 1)) <= ternary(stage_valid(l - 1)(n * 2 + 0) = '1',
                                                                          '0' & stage_enc(l - 1)((n * 2 + 1) * l - 1 downto (n * 2 + 0) * l),
                                                                          '1' & stage_enc(l - 1)((n * 2 + 2) * l - 1 downto (n * 2 + 1) * l));
            end generate;

            g0 : if LSB_HIGH_PRIORITY = 0 generate
                -- bit 0 is lowest priority
                stage_enc(l)((n + 1) * (l + 1) - 1 downto n * (l + 1)) <= ternary(stage_valid(l - 1)(n * 2 + 1) = '1',
                                                                          '1' & stage_enc(l - 1)((n * 2 + 2) * l - 1 downto (n * 2 + 1) * l),
                                                                          '0' & stage_enc(l - 1)((n * 2 + 1) * l - 1 downto (n * 2 + 0) * l));
            end generate;
        end generate;
    end generate;

        process(stage_enc) begin
            for i in 0 to stage_enc'length-1 loop
                report "stage_enc(" & to_string(i) & ") = " & to_string(stage_enc(i));
            end loop;
        end process;

    output_valid <= '1' when unsigned(stage_valid(LEVELS - 1)) /= 0 else
                    '0';
    output_encoded   <= stage_enc(LEVELS - 1);
    output_unencoded <= std_logic_vector(to_unsigned(1, WIDTH) sll to_integer(unsigned(stage_enc(LEVELS - 1))));

end architecture;