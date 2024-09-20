-- 
-- Copyright (c) 2014-2018 Alex Forencich
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
-- AXI4-Stream arbitrated multiplexer
-- 
entity axis_arb_mux is
    generic (
        -- Number of AXI stream inputs
        S_COUNT               : integer := 4;
        -- Width of AXI stream interfaces in bits
        DATA_WIDTH            : integer := 8;
        -- Propagate tkeep signal
        KEEP_ENABLE           : integer := ternary(DATA_WIDTH > 8, 1, 0);
        -- tkeep signal width (words per cycle)
        KEEP_WIDTH            : integer := ((DATA_WIDTH + 7)/8);
        -- Propagate tid signal
        ID_ENABLE             : integer := 0;
        -- input tid signal width
        S_ID_WIDTH            : integer := 8;
        -- output tid signal width
        M_ID_WIDTH            : integer := S_ID_WIDTH + clog2(S_COUNT);
        -- Propagate tdest signal
        DEST_ENABLE           : integer := 0;
        -- tdest signal width
        DEST_WIDTH            : integer := 8;
        -- Propagate tuser signal
        USER_ENABLE           : integer := 1;
        -- tuser signal width
        USER_WIDTH            : integer := 1;
        -- Propagate tlast signal
        LAST_ENABLE           : integer := 1;
        -- Update tid with routing information
        UPDATE_TID            : integer := 0;
        -- select round robin arbitration
        ARB_TYPE_ROUND_ROBIN  : integer := 0;
        -- LSB priority selection
        ARB_LSB_HIGH_PRIORITY : integer := 1
    );
    port (
        signal clk           : in  std_logic;
        signal rst           : in  std_logic;

        -- 
        -- AXI Stream inputs
        -- 
        signal s_axis_tdata  : in  std_logic_vector(S_COUNT * DATA_WIDTH - 1 downto 0);
        signal s_axis_tkeep  : in  std_logic_vector(S_COUNT * KEEP_WIDTH - 1 downto 0);
        signal s_axis_tvalid : in  std_logic_vector(S_COUNT - 1 downto 0);
        signal s_axis_tready : out std_logic_vector(S_COUNT - 1 downto 0);
        signal s_axis_tlast  : in  std_logic_vector(S_COUNT - 1 downto 0);
        signal s_axis_tid    : in  std_logic_vector(S_COUNT * S_ID_WIDTH - 1 downto 0);
        signal s_axis_tdest  : in  std_logic_vector(S_COUNT * DEST_WIDTH - 1 downto 0);
        signal s_axis_tuser  : in  std_logic_vector(S_COUNT * USER_WIDTH - 1 downto 0);

        --
        -- AXI Stream output
        --
        signal m_axis_tdata  : out std_logic_vector(DATA_WIDTH - 1 downto 0);
        signal m_axis_tkeep  : out std_logic_vector(KEEP_WIDTH - 1 downto 0);
        signal m_axis_tvalid : out std_logic;
        signal m_axis_tready : in  std_logic;
        signal m_axis_tlast  : out std_logic;
        signal m_axis_tid    : out std_logic_vector(M_ID_WIDTH - 1 downto 0);
        signal m_axis_tdest  : out std_logic_vector(DEST_WIDTH - 1 downto 0);
        signal m_axis_tuser  : out std_logic_vector(USER_WIDTH - 1 downto 0)
    );
end entity;

architecture rtl of axis_arb_mux is

    constant CL_S_COUNT            : integer := clog2(S_COUNT);

    constant S_ID_WIDTH_INT        : integer := ternary(S_ID_WIDTH > 0, S_ID_WIDTH, 1);

    signal request                 : std_logic_vector(S_COUNT - 1 downto 0);
    signal acknowledge             : std_logic_vector(S_COUNT - 1 downto 0);
    signal grant                   : std_logic_vector(S_COUNT - 1 downto 0);
    signal grant_valid             : std_logic;
    signal grant_encoded           : std_logic_vector(CL_S_COUNT - 1 downto 0);

    -- input registers to pipeline arbitration delay
    signal s_axis_tdata_reg        : std_logic_vector(S_COUNT * DATA_WIDTH - 1 downto 0) := (others => '0');
    signal s_axis_tkeep_reg        : std_logic_vector(S_COUNT * KEEP_WIDTH - 1 downto 0) := (others => '0');
    signal s_axis_tvalid_reg       : std_logic_vector(S_COUNT - 1 downto 0)              := (others => '0');
    signal s_axis_tlast_reg        : std_logic_vector(S_COUNT - 1 downto 0)              := (others => '0');
    signal s_axis_tid_reg          : std_logic_vector(S_COUNT * S_ID_WIDTH - 1 downto 0) := (others => '0');
    signal s_axis_tdest_reg        : std_logic_vector(S_COUNT * DEST_WIDTH - 1 downto 0) := (others => '0');
    signal s_axis_tuser_reg        : std_logic_vector(S_COUNT * USER_WIDTH - 1 downto 0) := (others => '0');

    -- internal datapath
    signal m_axis_tdata_int        : std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal m_axis_tkeep_int        : std_logic_vector(KEEP_WIDTH - 1 downto 0);
    signal m_axis_tvalid_int       : std_logic;
    signal m_axis_tready_int_reg   : std_logic := '0';
    signal m_axis_tlast_int        : std_logic;
    signal m_axis_tid_int          : std_logic_vector(M_ID_WIDTH - 1 downto 0);
    signal m_axis_tdest_int        : std_logic_vector(DEST_WIDTH - 1 downto 0);
    signal m_axis_tuser_int        : std_logic_vector(USER_WIDTH - 1 downto 0);
    signal m_axis_tready_int_early : std_logic;

    -- mux for incoming packet
    signal current_s_tdata         : std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal current_s_tkeep         : std_logic_vector(KEEP_WIDTH - 1 downto 0);
    signal current_s_tvalid        : std_logic;
    signal current_s_tready        : std_logic;
    signal current_s_tlast         : std_logic;
    signal current_s_tid           : std_logic_vector(S_ID_WIDTH - 1 downto 0);
    signal current_s_tdest         : std_logic_vector(DEST_WIDTH - 1 downto 0);
    signal current_s_tuser         : std_logic_vector(USER_WIDTH - 1 downto 0);

    function mask_and_or(w         : integer; v : std_logic; value : std_logic_vector) return std_logic is
        variable const                 : std_logic_vector(w - 1 downto 0) := (others => v);
    begin
        return or(const and value);
    end function;

    -- output datapath logic
    signal m_axis_tdata_reg          : std_logic_vector(DATA_WIDTH - 1 downto 0) := (others => '0');
    signal m_axis_tkeep_reg          : std_logic_vector(KEEP_WIDTH - 1 downto 0) := (others => '0');
    signal m_axis_tvalid_reg         : std_logic                                 := '0';
    signal m_axis_tvalid_next        : std_logic;
    signal m_axis_tlast_reg          : std_logic                                 := '0';
    signal m_axis_tid_reg            : std_logic_vector(M_ID_WIDTH - 1 downto 0) := (others => '0');
    signal m_axis_tdest_reg          : std_logic_vector(DEST_WIDTH - 1 downto 0) := (others => '0');
    signal m_axis_tuser_reg          : std_logic_vector(USER_WIDTH - 1 downto 0) := (others => '0');

    signal temp_m_axis_tdata_reg     : std_logic_vector(DATA_WIDTH - 1 downto 0) := (others => '0');
    signal temp_m_axis_tkeep_reg     : std_logic_vector(KEEP_WIDTH - 1 downto 0) := (others => '0');
    signal temp_m_axis_tvalid_reg    : std_logic                                 := '0';
    signal temp_m_axis_tvalid_next   : std_logic;
    signal temp_m_axis_tlast_reg     : std_logic                                 := '0';
    signal temp_m_axis_tid_reg       : std_logic_vector(M_ID_WIDTH - 1 downto 0) := (others => '0');
    signal temp_m_axis_tdest_reg     : std_logic_vector(DEST_WIDTH - 1 downto 0) := (others => '0');
    signal temp_m_axis_tuser_reg     : std_logic_vector(USER_WIDTH - 1 downto 0) := (others => '0');

    -- datapath control
    signal store_axis_int_to_output  : std_logic;
    signal store_axis_int_to_temp    : std_logic;
    signal store_axis_temp_to_output : std_logic;
begin

    -- check configuration
    P_check : process (all) begin
        if UPDATE_TID /= 0 then
            if ID_ENABLE = 0 then
                report "Error: UPDATE_TID set requires ID_ENABLE set"
                    severity failure;
            end if;

            if M_ID_WIDTH < CL_S_COUNT then
                report "Error: M_ID_WIDTH too small for port count"
                    severity failure;
            end if;
        end if;
    end process;

    -- assign s_axis_tredy = ~s_axis_tvalid_reg | ({S_COUNT{m_axis_tready_int_reg}} & grant);
    s_axis_tready <= not(s_axis_tvalid_reg) or (const_v(S_COUNT, m_axis_tready_int_reg) and grant);

    process (all) begin
        current_s_tdata  <= s_axis_tdata_reg((to_integer(unsigned(grant_encoded)) + 1) * DATA_WIDTH - 1 downto to_integer(unsigned(grant_encoded)) * DATA_WIDTH);
        current_s_tkeep  <= s_axis_tkeep_reg((to_integer(unsigned(grant_encoded)) + 1) * KEEP_WIDTH - 1 downto to_integer(unsigned(grant_encoded)) * KEEP_WIDTH);
        current_s_tvalid <= s_axis_tvalid_reg(to_integer(unsigned(grant_encoded)));
        current_s_tready <= s_axis_tready(to_integer(unsigned(grant_encoded)));
        current_s_tlast  <= s_axis_tlast_reg(to_integer(unsigned(grant_encoded)));
        current_s_tid    <= s_axis_tid_reg(to_integer(unsigned(grant_encoded)) * S_ID_WIDTH + S_ID_WIDTH_INT - 1 downto to_integer(unsigned(grant_encoded)) * S_ID_WIDTH);
        current_s_tdest  <= s_axis_tdest_reg((to_integer(unsigned(grant_encoded)) + 1) * DEST_WIDTH - 1 downto to_integer(unsigned(grant_encoded)) * DEST_WIDTH);
        current_s_tuser  <= s_axis_tuser_reg((to_integer(unsigned(grant_encoded)) + 1) * USER_WIDTH - 1 downto to_integer(unsigned(grant_encoded)) * USER_WIDTH);
    end process;

    -- arbiter instance
    arb_inst : entity work.arbiter
        generic map(
            PORTS                 => S_COUNT,
            ARB_TYPE_ROUND_ROBIN  => ARB_TYPE_ROUND_ROBIN,
            ARB_BLOCK             => 1,
            ARB_BLOCK_ACK         => 1,
            ARB_LSB_HIGH_PRIORITY => ARB_LSB_HIGH_PRIORITY
        )
        port map(
            clk           => clk,
            rst           => rst,
            request       => request,
            acknowledge   => acknowledge,
            grant         => grant,
            grant_valid   => grant_valid,
            grant_encoded => grant_encoded
        );

    request     <= (s_axis_tvalid_reg and not(grant)) or (s_axis_tvalid and grant);
    acknowledge <= grant and s_axis_tvalid_reg and const_V(S_COUNT, m_axis_tready_int_reg) and ternary(LAST_ENABLE, s_axis_tlast_reg, const_1(S_COUNT));

    process (all) begin
        -- pass through selected packet data
        m_axis_tdata_int  <= current_s_tdata;
        m_axis_tkeep_int  <= current_s_tkeep;
        m_axis_tvalid_int <= current_s_tvalid and m_axis_tready_int_reg and grant_valid;
        m_axis_tlast_int  <= current_s_tlast;
        m_axis_tid_int    <= std_logic_vector(resize(unsigned(current_s_tid), m_axis_tid_int'length));

        if UPDATE_TID /= 0 and S_COUNT > 1 then
            m_axis_tid_int(M_ID_WIDTH - 1 downto M_ID_WIDTH - CL_S_COUNT) <= grant_encoded;
        end if;

        m_axis_tdest_int <= current_s_tdest;
        m_axis_tuser_int <= current_s_tuser;
    end process;

    process (clk) begin
        -- register inputs
        if rising_edge(clk) then
            for i in 0 to S_COUNT - 1 loop
                if s_axis_tready(i) = '1' then
                    s_axis_tdata_reg((i + 1) * DATA_WIDTH - 1 downto i * DATA_WIDTH)   <= s_axis_tdata((i + 1) * DATA_WIDTH - 1 downto i * DATA_WIDTH);
                    s_axis_tkeep_reg((i + 1) * KEEP_WIDTH - 1 downto i * KEEP_WIDTH)   <= s_axis_tkeep((i + 1) * KEEP_WIDTH - 1 downto i * KEEP_WIDTH);
                    s_axis_tvalid_reg(i)                                               <= s_axis_tvalid(i);
                    s_axis_tlast_reg(i)                                                <= s_axis_tlast(i);
                    s_axis_tid_reg((i + 1) * S_ID_WIDTH - 1 downto i * S_ID_WIDTH_INT) <= s_axis_tid((i + 1) * S_ID_WIDTH - 1 downto i * S_ID_WIDTH_INT);
                    s_axis_tdest_reg((i + 1) * DEST_WIDTH - 1 downto i * DEST_WIDTH)   <= s_axis_tdest((i + 1) * DEST_WIDTH - 1 downto i * DEST_WIDTH);
                    s_axis_tuser_reg((i + 1) * USER_WIDTH - 1 downto i * USER_WIDTH)   <= s_axis_tuser((i + 1) * USER_WIDTH - 1 downto i * USER_WIDTH);
                end if;
            end loop;

            if rst = '1' then
                s_axis_tvalid_reg <= (others => '0');
            end if;
        end if;
    end process;

    -- datapath control
    m_axis_tdata            <= m_axis_tdata_reg;
    m_axis_tkeep            <= ternary(KEEP_ENABLE, m_axis_tkeep_reg, const_1(KEEP_WIDTH));
    m_axis_tvalid           <= m_axis_tvalid_reg;
    m_axis_tlast            <= ternary(LAST_ENABLE, m_axis_tlast_reg, '1');
    m_axis_tid              <= ternary(ID_ENABLE, m_axis_tid_reg, const_0(M_ID_WIDTH));
    m_axis_tdest            <= ternary(DEST_ENABLE, m_axis_tdest_reg, const_0(DEST_WIDTH));
    m_axis_tuser            <= ternary(USER_ENABLE, m_axis_tuser_reg, const_0(USER_WIDTH));

    -- enable ready input next cycle if output is ready or the temp reg will not be filled on the next cycle (output reg empty or no input)
    m_axis_tready_int_early <= m_axis_tready or (not temp_m_axis_tvalid_reg and (not m_axis_tvalid_reg or not m_axis_tvalid_int));

    process (all) begin
        -- transfer sink ready state to source
        m_axis_tvalid_next        <= m_axis_tvalid_reg;
        temp_m_axis_tvalid_next   <= temp_m_axis_tvalid_reg;

        store_axis_int_to_output  <= '0';
        store_axis_int_to_temp    <= '0';
        store_axis_temp_to_output <= '0';

        if m_axis_tready_int_reg = '1' then
            -- input is ready
            if m_axis_tready = '1' or m_axis_tvalid_reg = '0' then
                -- output is ready or currently not valid, transfer data to output
                m_axis_tvalid_next       <= m_axis_tvalid_int;
                store_axis_int_to_output <= '1';
            else
                -- output is not ready, store input in temp
                temp_m_axis_tvalid_next <= m_axis_tvalid_int;
                store_axis_int_to_temp  <= '1';
            end if;
        elsif m_axis_tready = '1' then
            -- input is not ready, but output is ready
            m_axis_tvalid_next        <= temp_m_axis_tvalid_reg;
            temp_m_axis_tvalid_next   <= '0';
            store_axis_temp_to_output <= '1';
        end if;
    end process;

    process (clk) begin
        if rising_edge(clk) then
            m_axis_tvalid_reg      <= m_axis_tvalid_next;
            m_axis_tready_int_reg  <= m_axis_tready_int_early;
            temp_m_axis_tvalid_reg <= temp_m_axis_tvalid_next;

            -- datapath
            if store_axis_int_to_output = '1' then
                m_axis_tdata_reg <= m_axis_tdata_int;
                m_axis_tkeep_reg <= m_axis_tkeep_int;
                m_axis_tlast_reg <= m_axis_tlast_int;
                m_axis_tid_reg   <= m_axis_tid_int;
                m_axis_tdest_reg <= m_axis_tdest_int;
                m_axis_tuser_reg <= m_axis_tuser_int;
            elsif store_axis_temp_to_output = '1' then
                m_axis_tdata_reg <= temp_m_axis_tdata_reg;
                m_axis_tkeep_reg <= temp_m_axis_tkeep_reg;
                m_axis_tlast_reg <= temp_m_axis_tlast_reg;
                m_axis_tid_reg   <= temp_m_axis_tid_reg;
                m_axis_tdest_reg <= temp_m_axis_tdest_reg;
                m_axis_tuser_reg <= temp_m_axis_tuser_reg;
            end if;

            if store_axis_int_to_temp = '1' then
                temp_m_axis_tdata_reg <= m_axis_tdata_int;
                temp_m_axis_tkeep_reg <= m_axis_tkeep_int;
                temp_m_axis_tlast_reg <= m_axis_tlast_int;
                temp_m_axis_tid_reg   <= m_axis_tid_int;
                temp_m_axis_tdest_reg <= m_axis_tdest_int;
                temp_m_axis_tuser_reg <= m_axis_tuser_int;
            end if;

            if rst = '1' then
                m_axis_tvalid_reg      <= '0';
                m_axis_tready_int_reg  <= '0';
                temp_m_axis_tvalid_reg <= '0';
            end if;
        end if;
    end process;

end architecture;