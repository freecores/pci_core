-------------------------------------------------------------------------------
-- Title      : PCI Parity test bench
-- Project    : PCI target Core
-------------------------------------------------------------------------------
-- File        : pci_parity_tb.VHD
-- Author      : Jamil Khatib  <khatib@ieee.org>
-- Organization: OpenCores Project
-- Created     : 2000/04/1
-- Last update : 2000/04/1
-- Platform    : 
-- Simulators  : Modelsim 5.3 XE / Windows98
-- Synthesizers: 
-- Target      : 
-- Dependency  : 
-------------------------------------------------------------------------------
-- Description: PCI Parity test bench
-------------------------------------------------------------------------------
-- Copyright (c) 2000 Jamil Khatib
-- 
-- This VHDL design file is an open design; you can redistribute it and/or
-- modify it and/or implement it under the terms of the Openip General Public
-- License as it is going to be published by the OpenIPCore Organization and
-- any coming versions of this license.
-- You can check the draft license at
-- http://www.openip.org/oc/license.html
--
-------------------------------------------------------------------------------
-- Revisions  :
-- Revision Number : 1
-- Version         :   1.0
-- Date            :   2st Apr 2000
-- Modifier        :   Jamil Khatib (khatib@ieee.org)
-- Desccription    :   Created
--
-- Known bugs      :
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity parity_tb is

end parity_tb;

architecture behavior_tb of parity_tb is
  component parity

    port (
      -- PCI Interface 
      CLK          : in    std_logic;   -- PCI clock
      AD           : in    std_logic_vector(31 downto 0);  -- PCI AD signal
      CBE          : in    std_logic_vector(3 downto 0);  -- C/BE PCI bus signals
      PAR          : inout std_logic;   -- PAR signal
      SERR_n       : inout std_logic;   -- SERR# signal
      PERR_n       : out   std_logic;   -- PERR# signal
                                        -- PERR# signal is output only for target
      -- Local Interface
      ParOperation : in    std_logic;   -- Parity Operation
                                        -- Drive PAR or check it
      Par_oe       : in    std_logic;   -- PAR Output Enable
      Locserr_n    : in    std_logic;   -- Local System Error
      LocErrRep_n  : out   std_logic);  -- Local Error Report
                                        -- used to report parity errors for local interface
                                        -- and to the configuration register
  end component;

  signal clk_tb  : std_logic := '0';               -- clock
  signal ad_tb   : std_logic_vector(31 downto 0);  -- AD signal
  signal cbe_tb  : std_logic_vector(3 downto 0);   -- C/BE signal
  signal par_tb  : std_logic;
  signal serr_tb : std_logic;
  signal perr_tb : std_logic;

  signal paroperation_tb : std_logic;
  signal paroe_tb        : std_logic;
  signal locserr_tb      : std_logic;
  signal locerrrep_tb    : std_logic;

  signal par_syn_tb       : std_logic;
  signal serr_syn_tb      : std_logic;
  signal perr_syn_tb      : std_logic;
  signal locerrrep_syn_tb : std_logic;

begin  -- behavior_tb

  clk_tb <= not clk_tb after 30 ns;

  UUT : parity
    port map (
      CLK          => clk_tb,
      AD           => ad_tb,
      CBE          => cbe_tb,
      PAR          => par_tb,
      SERR_n       => serr_tb,
      PERR_n       => perr_tb,
      ParOperation => paroperation_tb,
      Par_oe       => paroe_tb,
      Locserr_n    => locserr_tb,
      LocErrRep_n  => locerrrep_tb);
-------------------------------------------------------------------------------

  UUT_syn : parity
    port map (
      CLK          => clk_tb,
      AD           => ad_tb,
      CBE          => cbe_tb,
      PAR          => par_syn_tb,
      SERR_n       => serr_syn_tb,
      PERR_n       => perr_syn_tb,
      ParOperation => paroperation_tb,
      Par_oe       => paroe_tb,
      Locserr_n    => locserr_tb,
      LocErrRep_n  => locerrrep_syn_tb);


  par_tb <= transport 'Z'    after 0 ns,
                       '1'   after 400 ns;

  par_syn_tb <= transport 'Z'    after 0 ns,
                       '1'   after 400 ns;
  
  ad_tb  <= (others => '1');
  cbe_tb <= transport "1111" after 0 ns,
            "1000"           after 200 ns,
            "1111"           after 400 ns,
            "1000"           after 600 ns;

  paroperation_tb <= transport '1' after 0 ns,
                     '0'           after 400 ns;
  paroe_tb        <= transport '1' after 0 ns,
                     '0'           after 800 ns;
  locserr_tb      <= '1';



end behavior_tb;

-- Test bench Configuration
configuration TESTBENCH_FOR_parity of parity_tb is
  for behavior_tb
    for UUT : parity
      use entity work.parity(behavior);
    end for;

    for UUT_syn : parity
      use entity work.parity(STRUCTURE);
    end for;

  end for;
end TESTBENCH_FOR_parity;
