-------------------------------------------------------------------------------
-- Title      : PCI Parity core
-- Project    : PCI target Core
-------------------------------------------------------------------------------
-- File        : pci_parity.VHD
-- Author      : Jamil Khatib  <khatib@ieee.org>
-- Organization: OpenCores Project
-- Created     : 2000/04/1
-- Last update : 2000/04/1
-- Platform    : 
-- Simulators  : Modelsim 5.3XE / Windows98
-- Synthesizers: webfitter - Leonardo / WindowsNT
-- Target      : XC9572XL-5-VQ64 - EPF10K100EQC208 Flex10K
-- Dependency  : 
-------------------------------------------------------------------------------
-- Description: PCI Parity Core
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
-- Date            :   1st Apr 2000
-- Modifier        :   Jamil Khatib (khatib@ieee.org)
-- Desccription    :   Created
--
-- Known bugs      : Extending the PAR signals to wait states
--                 : SERR is generated upon local side request only
--                 : PERR must remain active two clockcycles after the ERR
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity parity is

  port (
    -- PCI Interface 
    CLK          : in    std_logic;     -- PCI clock
    AD           : in    std_logic_vector(31 downto 0);  -- PCI AD signal
    CBE          : in    std_logic_vector(3 downto 0);  -- C/BE PCI bus signals
    PAR          : inout std_logic;     -- PAR signal
    SERR_n       : inout std_logic;     -- SERR# signal
    PERR_n       : out   std_logic;     -- PERR# signal
                                        -- PERR# signal is output only for target
    -- Local Interface
    ParOperation : in    std_logic;     -- Parity Operation
                                        -- Drive PAR or check it
    Par_oe       : in    std_logic;     -- PAR Output Enable
    Locserr_n    : in    std_logic;     -- Local System Error
    LocErrRep_n  : out   std_logic);    -- Local Error Report
                                        -- used to report parity errors for local interface
                                        -- and to the configuration register

end parity;

library ieee;
use ieee.std_logic_1164.all;
-------------------------------------------------------------------------------
architecture behavior of parity is

begin  -- behavior

-------------------------------------------------------------------------------
-- purpose: Parity Generation
-- type   : sequential
-- inputs : CLK
-- outputs: PAR, LocErrRep
  Paritygen : process (CLK)

    variable tmp_par : std_logic;       -- temporary parity vriable
    variable par_q   : std_logic;       -- Next Par signal
    variable perr_q  : std_logic;       -- Next PERR signal

  begin  -- process Paritygen

    if CLK'event and CLK = '1' then     -- rising clock edge
      if Par_oe = '1' then

-------------------------------------------------------------------------------
--      PAR signal states:
--      Idel: when no operation on the current target or master
--          PAR = 'Z' , PERR = 'Z'
--      Master Read:
--        Address phase:
--                 Master Drives PAR
--                 Target Drives PERR
--        Data phase:
--                 Target Drives PAR
--                 Master Drives PERR
--      Master write:
--        Address and data phase
--                 Master Drives PAR
--                 Master Drives PERR
-------------------------------------------------------------------------------
--      ParOperation = 1 Calculate and drive PAR Port 
--      ParOperation = 0 Calculate and report Parity Errors
-------------------------------------------------------------------------------                                       
        if ParOperation = '1' then      -- Drive PAR signal

          PAR         <= par_q;
          LocErrRep_n <= perr_q;
          PERR_n <= 'Z';
          
        else

          PERR_n <= perr_q;
          PAR <= 'Z';

        end if;

-- No of 1's in AD, CBE & PAR must be even
        tmp_par := CBE(3) xor CBE(2) xor CBE(1) xor CBE(0);

        for i in AD'range loop

          tmp_par := tmp_par xor AD(i);

        end loop;  -- i

        par_q := tmp_par;

        perr_q := tmp_par xor PAR_q;

      else

        PAR    <= 'Z';
        PERR_n <= 'Z';

      end if;
    end if;
  end process Paritygen;
-------------------------------------------------------------------------------

  SERR_n <= Locserr_n;
-------------------------------------------------------------------------------
end behavior;
