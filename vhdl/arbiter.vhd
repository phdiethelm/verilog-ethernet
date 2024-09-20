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
-- Arbiter module
-- 
entity arbiter is
    generic (
        PORTS                 : integer := 4;
        -- select round robin arbitration
        ARB_TYPE_ROUND_ROBIN  : integer := 0;
        -- blocking arbiter enable
        ARB_BLOCK             : integer := 0;
        -- block on acknowledge assert when nonzero, request deassert when 0
        ARB_BLOCK_ACK         : integer := 1;
        -- LSB priority selection
        ARB_LSB_HIGH_PRIORITY : integer := 0
    );
    port (
        signal clk           : in  std_logic;
        signal rst           : in  std_logic;

        signal request       : in  std_logic_vector(PORTS - 1 downto 0);
        signal acknowledge   : in  std_logic_vector(PORTS - 1 downto 0);

        signal grant         : out std_logic_vector(PORTS - 1 downto 0);
        signal grant_valid   : out std_logic;
        signal grant_encoded : out std_logic_vector(clog2(PORTS) - 1 downto 0)
    );
end entity;

architecture rtl of arbiter is

    signal grant_reg            : std_logic_vector(PORTS - 1 downto 0) := (others => '0');
    signal grant_next           : std_logic_vector(PORTS - 1 downto 0);
    signal grant_valid_reg      : std_logic := '0';
    signal grant_valid_next     : std_logic;
    signal grant_encoded_reg    : std_logic_vector(clog2(PORTS) - 1 downto 0) := (others => '0');
    signal grant_encoded_next   : std_logic_vector(clog2(PORTS) - 1 downto 0);

    signal request_valid        : std_logic;
    signal request_index        : std_logic_vector(clog2(PORTS) - 1 downto 0);
    signal request_mask         : std_logic_vector(PORTS - 1 downto 0);

    signal mask_reg             : std_logic_vector(PORTS - 1 downto 0) := (others => '0');
    signal mask_next            : std_logic_vector(PORTS - 1 downto 0);

    signal masked_request_valid : std_logic;
    signal masked_request_index : std_logic_vector(clog2(PORTS) - 1 downto 0);
    signal masked_request_mask  : std_logic_vector(PORTS - 1 downto 0);

begin

    grant_valid   <= grant_valid_reg;
    grant         <= grant_reg;
    grant_encoded <= grant_encoded_reg;

    priority_encoder_inst : entity work.priority_encoder
        generic map(
            WIDTH             => PORTS,
            LSB_HIGH_PRIORITY => ARB_LSB_HIGH_PRIORITY
        )
        port map(
            input_unencoded  => request,
            output_valid     => request_valid,
            output_encoded   => request_index,
            output_unencoded => request_mask
        );

    priority_encoder_masked : entity work.priority_encoder
        generic map(
            WIDTH             => PORTS,
            LSB_HIGH_PRIORITY => ARB_LSB_HIGH_PRIORITY
        )
        port map(
            input_unencoded  => request and mask_reg,
            output_valid     => masked_request_valid,
            output_encoded   => masked_request_index,
            output_unencoded => masked_request_mask
        );

    process (all) begin
        grant_next         <= (others => '0');
        grant_valid_next   <= '0';
        grant_encoded_next <= (others => '0');
        mask_next          <= mask_reg;

        --  grant_reg & request
        if ARB_BLOCK /= 0 and ARB_BLOCK_ACK = 0 and mask_check(grant_reg, request, request)  then
            -- granted request still asserted; hold it
            grant_valid_next   <= grant_valid_reg;
            grant_next         <= grant_reg;
            grant_encoded_next <= grant_encoded_reg;
            -- !(grant_reg & acknowledge) => mask_check(grant_reg, acknowledge, const_0(PORTS))
        elsif ARB_BLOCK /= 0 and ARB_BLOCK_ACK /= 0 and grant_valid = '1' and mask_check(grant_reg, acknowledge, const_0(PORTS)) then
            -- granted request not yet acknowledged; hold it
            grant_valid_next   <= grant_valid_reg;
            grant_next         <= grant_reg;
            grant_encoded_next <= grant_encoded_reg;
        elsif request_valid = '1' then
            if ARB_TYPE_ROUND_ROBIN = 1 then
                if masked_request_valid = '1' then
                    grant_valid_next   <= '1';
                    grant_next         <= masked_request_mask;
                    grant_encoded_next <= masked_request_index;
                    if ARB_LSB_HIGH_PRIORITY = 1 then
                        mask_next <= const_1(PORTS) sll (to_integer(unsigned(masked_request_index)) + 1);
                    else
                        mask_next <= const_1(PORTS) srl (PORTS - to_integer(unsigned(masked_request_index)));
                    end if;
                else
                    grant_valid_next   <= '1';
                    grant_next         <= request_mask;
                    grant_encoded_next <= request_index;
                    if ARB_LSB_HIGH_PRIORITY = 1 then
                        mask_next <= const_1(PORTS) sll (to_integer(unsigned(request_index)) + 1);
                    else
                        mask_next <= const_1(PORTS) srl (PORTS - to_integer(unsigned(request_index)));
                    end if;
                end if;
            else
                grant_valid_next   <= '1';
                grant_next         <= request_mask;
                grant_encoded_next <= request_index;
            end if;
        end if;
    end process;

    process (clk, rst) begin
        if rst = '1' then
            grant_reg         <= (others => '0');
            grant_valid_reg   <= '0';
            grant_encoded_reg <= (others => '0');
            mask_reg          <= (others => '0');
        elsif rising_edge(clk) then
            grant_reg         <= grant_next;
            grant_valid_reg   <= grant_valid_next;
            grant_encoded_reg <= grant_encoded_next;
            mask_reg          <= mask_next;
        end if;
    end process;

end architecture;