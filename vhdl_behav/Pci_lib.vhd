--===================================================================--
--
--  www.OpenCores.Org - June 2000
--  This model adheres to the GNU public license  
--
-- Design units   : Functions and procedures used in models
--
-- File name      : PCI_LIB.vhd
--
-- Purpose        : type conversion functions, read commands file
--                  procedures
--
-- Limitations    : None known
--
-- Errors         : None known
--
-- Library        : PCI_Lib.vhd
--
-- Dependencies   : IEEE.Std_Logic_1164
--
-- Author         : Ovidiu Lupas
--                  olupas@opencores.org
--
-- Simulator      : ModelSim EE version 5.2 on a Windows95 PC
--                  ActiveVHDL 3.1 on a Windows95 PC
--===================================================================--
-----------------------------------------------------------------------
-- Revision list
-- Version     Author          Date           Changes
--
-- 0.1       Ovidiu Lupas   June 09, 2000     New model
-----------------------------------------------------------------------
library IEEE;
use IEEE.Std_Logic_1164.all;
--
package Simulation is
-- Definition of the CommandType record array which is used to record 
-- the commands from the .cmd file
   type Data_buffer is array(1 to 256) of Std_Logic_Vector(31 downto 0);
   type Data_Enable is array(1 to 256) of Std_Logic_Vector(3 downto 0);
   
   type CommandType is
        record
              command :   string(1 to 4);
              addr    :   Std_Logic_Vector(31 downto 0);
              data    :   Data_buffer;
              data_nr :   Integer;
              enable  :   Data_Enable;
        end record;
-- Definition of the CommandArray type type which can be used for
-- the commands present in .cmd file
   type CommandArray is array(positive range <>) of CommandType;
end Simulation; --========== End of package Simulation =============--
--
library IEEE,STD,work;
library work;
use work.Simulation.all;
use IEEE.Std_Logic_1164.all;
use STD.textio.all;
--
package PCI_Def is
   --------------------------------------------------------------------
   -- convert a character to a value from 0 to 15
   --------------------------------------------------------------------
   function digit_value(
        C : Character)
    return integer;
   --------------------------------------------------------------------
   -- Converts unsigned Std_LOGIC_Vector to Integer, leftmost bit is MSB
   -- Error message for unknowns (U, X, W, Z, -), converted to 0
   -- Verifies whether vector is too long (> 16 bits)
   --------------------------------------------------------------------
   function  Vec2Int (
        Invector : in Std_Logic_Vector(15 downto 0))
    return     Integer; 
   --------------------------------------------------------------------
   -- Converts unsigned Std_Logic_Vector to Integer, leftmost bit is MSB
   -- Error message for unknowns (U, X, W, Z, -), converted to 0
   -- Verifies whether vector is too long (> 16 bits)
   --------------------------------------------------------------------
   function  Byte2Int (
        Invector : in Std_Logic_Vector(7 downto 0))
    return     Integer; 
   --------------------------------------------------------------------
   -- Converts unsigned Std_Logic_Vector to Integer, leftmost bit is MSB
   -- Error message for unknowns (U, X, W, Z, -), converted to 0
   -- Verifies whether vector is too long (> 16 bits)
   --------------------------------------------------------------------
   procedure Hex2Bit (
        C        : in  Character;
        Vector   : out Std_Logic_Vector(3 downto 0);
        Good     : out Boolean);
   --------------------------------------------------------------------
   -- Converts Hex characters to binary representation
   -- Asserts that no unknown value exists at the time of conversion.
   --------------------------------------------------------------------
   procedure Bit2Hex (
        Vector   : in  Std_Logic_Vector(3 downto 0);		
        C        : out Character;
        Good     : out Boolean);
   --------------------------------------------------------------------
   -- Converts bit_vector into a hex string
   -- Asserts that no unknown value exists at the time of conversion.
   --------------------------------------------------------------------
   procedure Vec2Hex (
        Value  : in  Std_Logic_Vector(31 downto 0);
        result : out String(1 to 8);
        Good   : out Boolean);
   --------------------------------------------------------------------
   -- read a number from the line 
   -- use this if you have hex numbers that are not in VHDL pound-sign format
   --------------------------------------------------------------------
   procedure Read (
        L     : inout Line;
        value : out   Integer;
        radix : in    Positive);
   --------------------------------------------------------------------
   -- reads a hex value and returns a bit_vector value
   --------------------------------------------------------------------
   procedure ReadHex (
        L     : inout Line;
        Value : out   Std_Logic_Vector;
        Good  : out   Boolean;
        Enable: out   Std_Logic_Vector);
   --------------------------------------------------------------------
   -- Implements the parsing of the cmd file, which specifies the tasks
   -- that are applied to the processor.
   --------------------------------------------------------------------
   procedure FileParser (
        constant file_name   : in    String;
        variable Commands    : inout CommandArray;
        variable NumCommands : out   Natural;
        variable ErrFlag     : inout Boolean);
end PCI_Def; --============== End of package header =================--
--
library IEEE;
use IEEE.Std_Logic_1164.all;
library std;
use std.textio.all;
--
package body PCI_Def is
   --------------------------------------------------------------------
   -- convert a character to a value from 0 to 15
   --------------------------------------------------------------------
   function digit_value (
            C : Character)
      return integer is
       constant not_digit : integer := -999;
   begin
      if (C ='X') and (C ='x') then
         return 15;   
      end if;     
      if (C >= '0') and (C <= '9') then
         return (Character'pos(C) - Character'pos('0'));
      elsif (C >= 'a') and (C <= 'f') then
         return (character'pos(c) - character'pos('a') + 10);
      elsif (C >= 'A') and (C <= 'F') then
         return (character'pos(c) - character'pos('A') + 10);
      else
         return not_digit;
      end if;
   end digit_value;
   --------------------------------------------------------------------
   --------------------------------------------------------------------
   function  Vec2Int (
           InVector : in Std_Logic_Vector(15 downto 0))
      return  Integer is
     constant HeaderMsg   : String          := "To_Integer:";
     constant MsgSeverity : Severity_Level  := Warning;
     variable Value       : Integer         := 0;
   begin
      if InVector = "UUUUUUUUUUUUUUUU" then
         report HeaderMsg&"The input vector is of type 'U'. Converted to 0"
           severity MsgSeverity;
      elsif InVector = "XXXXXXXXXXXXXXXX" then
         report HeaderMsg&"The input vector is of type 'X'. Converted to 0"
           severity MsgSeverity;
      elsif InVector = "WWWWWWWWWWWWWWWW" then
         report HeaderMsg&"The input vector is of type 'W'. Converted to 0"
           severity MsgSeverity;
      elsif InVector = "ZZZZZZZZZZZZZZZZ" then
         report HeaderMsg&"The input vector is of type 'Z'. Converted to 0"
           severity MsgSeverity;
      else
         for i in 0 to 15 loop
           if (InVector(i) = '1') then
              Value := Value + (2**I);
           end if;
         end loop;
      end if;
      return Value;
   end Vec2Int;
   --------------------------------------------------------------------
   --------------------------------------------------------------------
   function  Byte2Int (
       InVector : in Std_Logic_Vector(7 downto 0))
       return  Integer is
      constant HeaderMsg   : String          := "To_Integer:";
      constant MsgSeverity : Severity_Level  := Warning;
      variable Value       : Integer         := 0;
   begin
      for i in 0 to 7 loop
        if (InVector(i) = '1') then
           Value := Value + (2**I);
        end if;
      end loop;
      return Value;
   end Byte2Int;
   --------------------------------------------------------------------
   --------------------------------------------------------------------
   procedure Hex2Bit (
        C      : in  Character;
        Vector : out Std_Logic_Vector(3 downto 0);
        Good   : out Boolean) is
      variable Good1       : Boolean        := false;
      constant HeaderMsg   : String         := "Hex2Bit:";
      constant MsgSeverity : Severity_Level := Error;
   begin
      Good := false;
      case C is
        when '0' => Vector := "0000"; Good1 := true;
        when '1' => Vector := "0001"; Good1 := true;
        when '2' => Vector := "0010"; Good1 := true;
        when '3' => Vector := "0011"; Good1 := true;
        when '4' => Vector := "0100"; Good1 := true;
        when '5' => Vector := "0101"; Good1 := true;
        when '6' => Vector := "0110"; Good1 := true;
        when '7' => Vector := "0111"; Good1 := true;
        when '8' => Vector := "1000"; Good1 := true;
        when '9' => Vector := "1001"; Good1 := true;
        when 'A'|'a' => Vector := "1010"; Good1 := true;
        when 'B'|'b' => Vector := "1011"; Good1 := true;
        when 'C'|'c' => Vector := "1100"; Good1 := true;
        when 'D'|'d' => Vector := "1101"; Good1 := true;
        when 'E'|'e' => Vector := "1110"; Good1 := true;
        when 'F'|'f' => Vector := "1111"; Good1 := true;
        -- extended for std_LOGIC --
        when 'U'|'u' => Vector := "UUUU"; Good1 := true;
        when 'X'|'x' => Vector := "1111"; Good1 := true;
        when 'Z'|'z' => Vector := "ZZZZ"; Good1 := true;
        when 'W'|'w' => Vector := "WWWW"; Good1 := true;
        when 'L'|'l' => Vector := "LLLL"; Good1 := true;
        when 'H'|'h' => Vector := "HHHH"; Good1 := true;
        when '-' => Vector := "----"; Good1 := true;
        when others => Good1 := false;
      end case;
      if not Good1 then
         Vector := "0000";
         report HeaderMsg&"Expected a Hex character (0-F)"
            severity MsgSeverity;
      end if;
      Good := Good1;    
   end Hex2Bit;
   --------------------------------------------------------------------
   --------------------------------------------------------------------
   procedure Bit2Hex (
        Vector : Std_Logic_Vector(3 downto 0);		
        C      : out Character;
        Good   : out Boolean) is
        variable Good1       : Boolean        := false;
        constant HeaderMsg   : String         := "Bit2Hex: ";
        constant MsgSeverity : Severity_Level := Error;
   begin
    Good := false;	
    case Vector is 
       when x"0" => C := '0'; Good1 := true;
       when x"1" => C := '1'; Good1 := true;
       when x"2" => C := '2'; Good1 := true;
       when x"3" => C := '3'; Good1 := true;
       when x"4" => C := '4'; Good1 := true;
       when x"5" => C := '5'; Good1 := true;
       when x"6" => C := '6'; Good1 := true;
       when x"7" => C := '7'; Good1 := true;
       when x"8" => C := '8'; Good1 := true;
       when x"9" => C := '9'; Good1 := true;
       when x"A" => C := 'A'; Good1 := true;
       when x"B" => C := 'B'; Good1 := true;
       when x"C" => C := 'C'; Good1 := true;
       when x"D" => C := 'D'; Good1 := true;
       when x"E" => C := 'E'; Good1 := true;
       when x"F" => C := 'F'; Good1 := true;
       when "ZZZZ" => C := 'Z'; Good1 := true;
       when others => Good1 := false;
    end case;
    if not Good1 then
       C := '0';
       report HeaderMsg&"Expected a Hex character (0-F)"
         severity MsgSeverity;
    end if;
    Good := Good1;        
   end Bit2Hex;
   --------------------------------------------------------------------
   --------------------------------------------------------------------
   procedure Vec2Hex (
      Value   : in   Std_Logic_Vector(31 downto 0);
      Result  : out  String(1 to 8); 
      GOOD    : out  BOOLEAN) is
      variable OK          : Boolean;
      variable C           : Character;
      constant NE          : Integer := value'length/4;	-- number of Hex chars
      variable BV          : Std_Logic_Vector(value'length-1 downto 0) := Value;
      variable Res         : String(1 to 8);
      constant HeaderMsg   : String := "Vec2Hex: ";
      constant MsgSeverity : Severity_Level	:= Error;
   begin
     if Value'length mod 4 /= 0 then	-- Testing if Vector is mod 4
        Good := false;
        report HeaderMsg&"The length of input vector is not modulo 4!"
           severity MsgSeverity;
        return;
     end if;
     Bit2Hex(BV(3 downto 0), C, OK);	-- Conversion of the 4 LSB bits
     if not OK then 
        Good := false;
        return;
     end if;
     Res := C & Res(2 to NE);	-- Places first Char in Result
     for i in 1 to NE-1 loop
        Bit2Hex(BV(4*i+3 downto 4*i), C, OK);	-- Converts next Char
        if not OK then
           Good := false;
           return;
        end if;
        Res := Res(1 to i) & C & Res(i+2 to NE);
     end loop;
     for i in 0 to NE-1 loop
         Result (1+i) := Res (NE-i);
     end loop;
     Good := true;
   end Vec2Hex; 
   --------------------------------------------------------------------
   --------------------------------------------------------------------
   procedure Read (
      L     : inout line;
      Value : out   integer;
      Radix : in    positive) is
      constant not_digit : integer := -999;
      variable Digit     : integer;
      variable Result    : integer := 0;
      variable Pos       : integer;
   begin
      -- calculate the value
      if (L'length <=0 ) then
         for i in Pos to L'right loop
             digit := digit_value(L(i));
             exit when (Digit = not_digit) or (digit >= radix);
             Result := Result * Radix + digit;
             Pos := Pos + 1;
         end loop;
         Value := Result;
      end if;
   end Read;
   --------------------------------------------------------------------
   --------------------------------------------------------------------
   procedure ReadHex (
      L      : inout Line; 
      Value  : out   Std_Logic_Vector;
      Good   : out   Boolean;
      Enable : out   Std_Logic_Vector) is
      variable OK          : Boolean;
      variable C           : Character;
      constant NE          : Integer := value'length/4;
      variable BV          : Std_Logic_Vector(value'length-1 downto 0);
      variable TEMP        : Std_Logic_Vector(value'length-1 downto 0);
      variable S           : String(1 to NE-1);
      constant HeaderMsg   : String := "Vec2Hex";
      constant MsgSeverity : Severity_Level := Warning;
    begin
      Enable(3 downto 0) :="0000";  
      if Value'length mod 4 /= 0 then	-- Testing if Vector is mod 4
         Good := false;
         report HeaderMsg&"The length of input vector is not modulo 4!"
            severity MsgSeverity;
         return;
      end if;
      loop
         read(L, C, OK);		-- Finds the first Hex Char
         exit when (((C /= ' ') and (C /= CR) and (C /= HT)) or
         (L'length = 0));
      end loop;
      Hex2Bit(C, BV( 3 downto 0), OK);	-- Converts first Hex Char to 4 Bits
      if C ='X' or C ='x' then
         Enable(3) :='1';
      end if;

      if not OK then 
         Good := false;
         return;
      end if;
      read(L, S, OK);			-- Reads the next three Hex Chars
      if not OK then
         Good := false;
         return;
      end if;
      for i in 1 to NE-1 loop
         if s(i) ='X' or s(i) ='x' then
            if i=1 then
               Enable(3):='1';
            end if;
            if i=2 or i=3 then
               Enable(2):='1';
            end if;   
            if i=4 or i=5 then
               Enable(1):='1';
            end if;
            if i=6 or i=7 then
               Enable(0):='1';
            end if;          
         end if;
         Hex2Bit(s(i), BV(4*i+3 downto 4*i), OK); -- Converts to BitVector
         if not OK then
            Good := false;
            return;
         end if;		
	  end loop;
      Good := true;
      
      -- for byte	
   	  if (value'length = 8 ) then
         for i in 0 to 3 loop
            TEMP(i) := BV(value'length-4+i);
         end loop;
         for i in 0 to 3 loop
            TEMP(i+4) := BV(value'length-8+i);
         end loop;
      end if;   	  
   	  
      -- for word
      if (value'length = 16 ) then
         for i in 0 to 3 loop
            TEMP(i) := BV(value'length-4+i);
         end loop;
         for i in 0 to 3 loop
            TEMP(i+4) := BV(value'length-8+i);
         end loop;
         for i in 0 to 3 loop
            TEMP(i+8) := BV(value'length-12+i);
         end loop;
         for i in 0 to 3 loop
            TEMP(i+12) := BV(value'length-16+i);
         end loop;
      end if;
 
      -- for DWORD
      if (value'length =32 ) then 
         for i in 0 to 3 loop
            TEMP(i) := BV(value'length-4+i);
         end loop;
         for i in 0 to 3 loop
            TEMP(i+4) := BV(value'length-8+i);
         end loop;
         for i in 0 to 3 loop
            TEMP(i+8) := BV(value'length-12+i);
         end loop;
         for i in 0 to 3 loop
            TEMP(i+12) := BV(value'length-16+i);
         end loop;
         for i in 0 to 3 loop
            TEMP(i+16) := BV(value'length-20+i);
         end loop;
         for i in 0 to 3 loop
            TEMP(i+20) := BV(value'length-24+i);
         end loop;
         for i in 0 to 3 loop
            TEMP(i+24) := BV(value'length-28+i);
         end loop;
         for i in 0 to 3 loop
            TEMP(i+28) := BV(value'length-32+i);
         end loop;
      end if;
    
      Value := TEMP;
   end ReadHex;
   --------------------------------------------------------------------
   --------------------------------------------------------------------
   procedure FileParser (
      constant file_name   : in    String ;
      variable Commands    : inout CommandArray;
      variable NumCommands : out   Natural;
      variable ErrFlag     : inout Boolean) is
      File     CmdFile     : text;
      constant HeaderMsg   : String := "FileParser:";
      constant MsgSeverity : Severity_Level	:= Error;
      variable L           : Line := NULL;
      variable Linenum     : Natural := 0;
      variable LblNum      : Natural := 0;
      variable CmdNum      : Natural := 0;
      variable EndOfFile   : Boolean := false;
      variable Good        : Boolean := false;
      variable Int         : Integer;
      variable Tm          : Time;
      variable C           : Character;
      variable Addr        : Std_Logic_Vector(31 downto 0);
      variable Data        : Std_Logic_Vector(31 downto 0);
      variable Enable      : Std_Logic_Vector(3 downto 0);
      variable Vect_count  : Std_Logic_Vector(7 downto 0);	
      variable Data_word   : Std_Logic_Vector(15 downto 0);
      variable Data_byte   : Std_Logic_Vector(7 downto 0);
      variable str4        : string(1 to 4);
      variable count_nr    : Integer;
      variable count       : Integer;
   begin
      ErrFlag := false;
      FILE_OPEN(CmdFile,file_name,READ_MODE);
      loop
        readline(CmdFile,L);
        EndOfFile := endfile(CmdFile);
        CmdNum := CmdNum + 1;
        LineNum := LineNum + 1;
        read(L, str4, Good);
        if not Good then
           CmdNum := CmdNum-1;
        else
           case str4 is
  	           when "WRSW" =>      -- write single word command
                  readhex(L,Addr,Good,Enable);
                  if not Good then
                     report HeaderMsg&"Invalid WRSW command in :"&file_name&", line #"& integer'image(LineNum) &",  "&"Address parameter should be 8 hex characters"
                        severity MsgSeverity;
                     ErrFlag := true;   
                  else
                     Good := true; 
                     readhex(L,Data_byte,Good,Enable);
                     count_nr := Byte2Int(Data_Byte);	
                     count := 0;
                     for I in 0 to count_nr-1 loop
                         readhex(L,Data,Good,Enable);
                         if (Good =true) then
                            count := count + 1;
                            commands(CmdNum).command       := "WRSW";
                            commands(CmdNum).addr          := Addr;
                            commands(CmdNum).data(count)   := Data;
                            commands(CmdNum).data_nr       := count_nr;
                            commands(CmdNum).enable(count) := Enable;
                         else
                            report HeaderMsg&"Invalid DATA in WRSW command ";
                         end if;
                     end loop;             
                  end if;

  	           when "RDSW" =>    -- read single word command
                  readhex(L,Addr,Good,Enable);
                  if not Good then
                     report HeaderMsg&"Invalid RDSW command in :"&file_name&", line #"& integer'image(LineNum) &",  "&"Address parameter should be 8 hex characters"
                        severity MsgSeverity;
                     ErrFlag := true;   
                  else
                     Good := true; 
                     readhex(L,Data_byte,Good,Enable);
                     count_nr :=Byte2Int(Data_Byte);	
                     count := 0;
                     for I in 0 to count_nr-1 loop
                         readhex(L,Data,Good,Enable);
                         if (Good =true) then
                            count := count + 1;
                            commands(CmdNum).command := "RDSW";
                            commands(CmdNum).addr	 := Addr;
                            commands(CmdNum).data(count) := Data;
                            commands(CmdNum).data_nr := count_nr;
                            commands(CmdNum).enable(count) := Enable;                               
                         else 
                            report HeaderMsg&"Invalid DATA in RDSW command ";
                         end if;
                     end loop;                         
                  end if;

  	           when "WRMW" =>	    -- write multiple words command
                  readhex(L,Addr,Good,Enable);
                  if not Good then
                     report HeaderMsg&"Invalid WRMW command in :"&file_name&", line #"& integer'image(LineNum) &",  "&"Address parameter should be 8 hex characters"
                        severity MsgSeverity;
                     ErrFlag := true;   
                  else
                     Good := true; 
                     readhex(L,Data_byte,Good,Enable);
                     count_nr :=Byte2Int(Data_Byte);	
                     count := 0;				     
                     for I in 0 to count_nr-1 loop
                         readhex(L,Data,Good,Enable);
                         if (Good =true) then
                            count := count + 1;
                            commands(CmdNum).command := "WRMW";
                            commands(CmdNum).addr	 := Addr;
                            commands(CmdNum).data(count) := Data;
                            commands(CmdNum).data_nr := count_nr;
                            commands(CmdNum).enable(count) := Enable;                               
                         else 
                            report HeaderMsg&"Invalid DATA in WRMW command ";
                         end if;
                     end loop;                         
                  end if;

               when "RDMW" =>		-- read multiple words command
                  readhex(L,Addr,Good,Enable);
                  if not Good then
                     report HeaderMsg&"Invalid RDMW command in :"&file_name&", line #"& integer'image(LineNum) &",  "&"Address parameter should be 8 hex characters"
                        severity MsgSeverity;
                     ErrFlag := true;   
                  else
                     Good := true; 
                     readhex(L,Data_Byte,Good,Enable);
                     count_nr :=Byte2Int(Data_Byte);	
                     count := 0;				     
                     for I in 0 to count_nr-1 loop
                         readhex(L,Data,Good,Enable);
                         if (Good =true) then
                            count := count + 1;
                            commands(CmdNum).command := "RDMW";
                            commands(CmdNum).addr	 := Addr;
                            commands(CmdNum).data(count) := Data;
                            commands(CmdNum).data_nr := count_nr;
                            commands(CmdNum).enable(count) := Enable;                               
                         else 
                            report HeaderMsg&"Invalid DATA in RDMW command ";
                         end if;
                     end loop;                         
                  end if;

               when "RDML" =>		-- read multiple words command
                  readhex(L,Addr,Good,Enable);
                  if not Good then
                     report HeaderMsg&"Invalid RDML command in :"&file_name&", line #"& integer'image(LineNum) &",  "&"Address parameter should be 8 hex characters"
                        severity MsgSeverity;
                     ErrFlag := true;   
                  else
                     Good := true; 
                     readhex(L,Data_Byte,Good,Enable);
                     count_nr :=Byte2Int(Data_Byte);	
                     count := 0;				     
                     for I in 0 to count_nr-1 loop
                         readhex(L,Data,Good,Enable);
                         if (Good =true) then
                            count := count + 1;
                            commands(CmdNum).command := "RDML";
                            commands(CmdNum).addr	 := Addr;
                            commands(CmdNum).data(count) := Data;
                            commands(CmdNum).data_nr := count_nr;
                            commands(CmdNum).enable(count) := Enable;                               
                         else 
                            report HeaderMsg&"Invalid DATA in RDML command ";
                         end if;
                     end loop;                         
                  end if;

  	           when "WCFG" =>		-- write to configuration space command
                  readhex(L,Addr,Good,Enable);
                  if not Good then
                     report HeaderMsg&"Invalid WCFG command in :"&file_name&", line #"& integer'image(LineNum) &",  "&"Address parameter should be 8 hex characters"
                        severity MsgSeverity;
                     ErrFlag := true;   
                  else
                     Good := true; 
                     readhex(L,Data_byte,Good,Enable);
                     count_nr :=Byte2Int(Data_Byte);	
                     count := 0;				     
                     for I in 0 to count_nr-1 loop
                         readhex(L,Data,Good,Enable);
                         if (Good =true) then
                            count := count + 1;
                            commands(CmdNum).command := "WCFG";
                            commands(CmdNum).addr	 := Addr;
                            commands(CmdNum).data(count) := Data;
                            commands(CmdNum).data_nr := count_nr;
                            commands(CmdNum).enable(count) := Enable;                               
                         else 
                            report HeaderMsg&"Invalid DATA in WCFG command ";
                          end if;
                     end loop;                         
                  end if;

               when "RCFG" =>		-- read from configuration space command
                  readhex(L,Addr,Good,Enable);
                  if not Good then
                     report HeaderMsg&"Invalid RCFG command in :"&file_name&", line #"& integer'image(LineNum) &",  "&"Address parameter should be 8 hex characters"
                        severity MsgSeverity;
                     ErrFlag := true;   
                  else
                     Good := true; 
                     readhex(L,Data_Byte,Good,Enable);
                     count_nr :=Byte2Int(Data_Byte);	
                     count := 0;				     
                     for I in 0 to count_nr-1 loop
                         readhex(L,Data,Good,Enable);
                         if (Good =true) then
                            count := count + 1;
                            commands(CmdNum).command := "RCFG";
                            commands(CmdNum).addr	 := Addr;
                            commands(CmdNum).data(count) := Data;
                            commands(CmdNum).data_nr := count_nr;
                            commands(CmdNum).enable(count) := Enable;                               
                         else 
                            report HeaderMsg&"Invalid DATA in RCFG command ";
                         end if;
                     end loop;                         
                  end if;

  	           when "WRIO" =>		-- write to I/O space command
                  readhex(L,Addr,Good,Enable);
                  if not Good then
                     report HeaderMsg&"Invalid WRIO command in :"&file_name&", line #"& integer'image(LineNum) &",  "&"Address parameter should be 8 hex characters"
                        severity MsgSeverity;
                     ErrFlag := true;   
                  else
                     Good := true; 
                     readhex(L,Data_byte,Good,Enable);
                     count_nr :=Byte2Int(Data_Byte);	
                     count := 0;				     
                     for I in 0 to count_nr-1 loop
                         readhex(L,Data,Good,Enable);
                         if (Good = true) then
                            count := count + 1;
                            commands(CmdNum).command := "WRIO";
                            commands(CmdNum).addr	 := Addr;
                            commands(CmdNum).data(count) := Data;
                            commands(CmdNum).data_nr := count_nr;
                            commands(CmdNum).enable(count) := Enable;                               
                         else 
                            report HeaderMsg&"Invalid DATA in WRIO command ";
                         end if;
                     end loop;                         
                  end if;

               when "RDIO" =>		-- read from I/O space command
                  readhex(L,Addr,Good,Enable);
                  if not Good then
                     report HeaderMsg&"Invalid RDIO command in :"&file_name&", line #"& integer'image(LineNum) &",  "&"Address parameter should be 8 hex characters"
                        severity MsgSeverity;
                     ErrFlag := true;   
                  else
                     Good := true; 
                     readhex(L,Data_Byte,Good,Enable);
                     count_nr :=Byte2Int(Data_Byte);	
                     count := 0;				     
                     for I in 0 to count_nr-1 loop
                         readhex(L,Data,Good,Enable);
                         if (Good = true) then
			                count := count + 1;
                            commands(CmdNum).command := "RDIO";
                            commands(CmdNum).addr	 := Addr;
                            commands(CmdNum).data(count) := Data;
                            commands(CmdNum).data_nr := count_nr;
                            commands(CmdNum).enable(count) := Enable;                               
                         else 
                            report HeaderMsg&"Invalid DATA in RDIO command ";
                         end if;
                     end loop;                         
                  end if;

               when "CWAT" =>
                  readhex(L,Addr,Good,Enable);
                  if not Good then
                     report HeaderMsg&"Invalid CWAT command in :"&file_name&", line #"& integer'image(LineNum) &",  "&"Address parameter should be 8 hex characters"
                        severity MsgSeverity;
                     ErrFlag := true;
                  else
                     count_nr :=0;
                     readhex(L,Data,Good,Enable);
                     if (Good = true) then
                        count :=count +1;
                        commands(CmdNum).command := "WAIT";
                        commands(CmdNum).addr	 := Addr;
                        commands(CmdNum).data(1) := Data;
                        commands(CmdNum).data_nr := 1;
                        commands(CmdNum).enable(1) := Enable;                               
                     else 
                        report HeaderMsg&"Invalid DATA in CWAT command ";
                     end if;
                  end if;

               when "ABRT" =>
                  count :=count + 1;
                  commands(CmdNum).command := "ABRT";
                  commands(CmdNum).addr	 := "00000000000000000000000000000000";
                  commands(CmdNum).data(1) := "00000000000000000000000000000000";
                  commands(CmdNum).data_nr := 1;
                  commands(CmdNum).enable(1) := Enable;

               when others =>
                  CmdNum := CmdNum -1;
                  if str4(1 to 2)/= "##" then
                     report HeaderMsg & "Invalid command in file: "&file_name&",  line #"& integer'image(LineNum) &",  "& "Unknown command : "& str4 & l(l'left to l'right)
                        severity error;
                     ErrFlag := true;
                  end if;
           end case;
        end if;
        NumCommands := CmdNum;
        Exit when EndOfFile;
      end loop;
   end FileParser;
   --------------------------------------------------------------------
   --------------------------------------------------------------------
end PCI_Def; --================ End of package body ================--
