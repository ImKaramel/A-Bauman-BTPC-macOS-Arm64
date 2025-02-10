(******************************************************************************
 *                                 BeRoTinyPascal                             *
 ******************************************************************************
 *   A self-hosting capable tiny pascal compiler for the Win32 x86 platform   *
 ******************************************************************************
 *                        Version 2016-06-22-18-07-0000                       *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2006-2016, Benjamin Rosseaux (benjamin@rosseaux.com)         *
 *                                                                            *
 * This software is provided 'as-is', without any express or implied          *
 * warranty. In no event will the authors be held liable for any damages      *
 * arising from the use of this software.                                     *
 *                                                                            *
 * Permission is granted to anyone to use this software for any purpose,      *
 * including commercial applications, and to alter it and redistribute it     *
 * freely, subject to the following restrictions:                             *
 *                                                                            *
 * 1. The origin of this software must not be misrepresented; you must not    *
 *    claim that you wrote the original software. If you use this software    *
 *    in a product, an acknowledgement in the product documentation would be  *
 *    appreciated but is not required.                                        *
 * 2. Altered source versions must be plainly marked as such, and must not be *
 *    misrepresented as being the original software.                          *
 * 3. This notice may not be removed or altered from any source distribution. *
 *                                                                            *
 ******************************************************************************
 *                  General guidelines for code contributors                  *
 *============================================================================*
 *                                                                            *
 * 1. Make sure you are legally allowed to make a contribution under the zlib *
 *    license.                                                                *
 * 2. The zlib license header goes at the top of each source file, with       *
 *    appropriate copyright notice.                                           *
 * 3. After a pull request, check the status of your pull request on          *
      http://github.com/BeRo1985/berotinypascal .                             *
 * 4. Write code, which is compatible with Delphi 7-XE7 and FreePascal >= 3.0 *
 *    and even with BeRoTinyPascal itself, so don't use generics/templates,   *
 *    operator overloading and another newer syntax features than Delphi 7    *
 *    and BeRoTinyPascal have support for that.                               *
 * 5. Don't use any libraries/units except the RTL system unit functions      *
 * +. Make sure the code compiles with Delphi 7, FreePascal >= 3.0 and with   *
 *    BeRoTinyPascal itself.                                                  *
 *                                                                            *
 ******************************************************************************)
program BTPC; {{ BeRoTinyPascalCompiler }
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$ifdef Win32}
 {$define Windows}
{$endif}
{$ifdef Win64}
 {$define Windows}
{$endif}
{$ifdef WinCE}
 {$define Windows}
{$endif}
{$ifdef Windows}
 {$apptype console}
{$endif}
{$ifdef Darwin}
  {$define MacOS}
{$endif}

const MaximalCodeSize=262144;
      MaximalIdentifiers=512;
      MaximalTypes=32;
      MaximalList=10;
      MaximalAlfa=20;
      MaximalStringLength=255;
      MaximalCases=256;

      OPNone=-1;
      OPAdd=0;
      OPNeg=1;
      OPMul=2;
      OPDivD=3;
      OPRemD=4;
      OPDiv2=5;
      OPRem2=6;
      OPEqlI=7;
      OPNEqI=8;
      OPLssI=9;
      OPLeqI=10;
      OPGtrI=11;
      OPGEqI=12;
      OPDupl=13;
      OPSwap=14;
      OPAndB=15;
      OPOrB=16;
      OPLoad=17;
      OPStore=18;
      OPHalt=19;
      OPWrI=20;
      OPWrC=21;
      OPWrL=22;
      OPRdI=23;
      OPRdC=24;
      OPRdL=25;
      OPEOF=26;
      OPEOL=27;
      OPLdC=28;
      OPLdA=29;
      OPLdLA=30;
      OPLdL=31;
      OPLdG=32;
      OPStL=33;
      OPStG=34;
      OPMove=35;
      OPCopy=36;
      OPAddC=37;
      OPMulC=38;
      OPJmp=39;
      OPJZ=40;
      OPCall=41;
      OPAdjS=42;
      OPExit=43;

      TokIdent=0;
      TokNumber=1;
      TokStrC=2;
      TokPlus=3;
      TokMinus=4;
      TokMul=5;
      TokLBracket=6;
      TokRBracket=7;
      TokColon=8;
      TokEql=9;
      TokNEq=10;
      TokLss=11;
      TokLEq=12;
      TokGtr=13;
      TokGEq=14;
      TokLParent=15;
      TokRParent=16;
      TokComma=17;
      TokSemi=18;
      TokPeriod=19;
      TokAssign=20;
      SymBEGIN=21;
      SymEND=22;
      SymIF=23;
      SymTHEN=24;
      SymELSE=25;
      SymWHILE=26;
      SymDO=27;
      SymCASE=28;
      SymREPEAT=29;
      SymUNTIL=30;
      SymFOR=31;
      SymTO=32;
      SymDOWNTO=33;
      SymNOT=34;
      SymDIV=35;
      SymMOD=36;
      SymAND=37;
      SymOR=38;
      SymCONST=39;
      SymVAR=40;
      SymTYPE=41;
      SymARRAY=42;
      SymOF=43;
      SymPACKED=44;
      SymRECORD=45;
      SymPROGRAM=46;
      SymFORWARD=47;
      SymHALT=48;
      SymFUNC=49;
      SymPROC=50;

      IdCONST=0;
      IdVAR=1;
      IdFIELD=2;
      IdTYPE=3;
      IdFUNC=4;

      KindSIMPLE=0;
      KindARRAY=1;
      KindRECORD=2;

      TypeINT=1;
      TypeBOOL=2;
      TypeCHAR=3;
      TypeSTR=4;

      FunCHR=0;
      FunORD=1;
      FunWRITE=2;
      FunWRITELN=3;
      FunREAD=4;
      FunREADLN=5;
      FunEOF=6;
      FunEOFLN=7;

type TAlfa=array[1:MaximalAlfa] of char;

     TIdent=record
      Name:TAlfa;
      Link:integer;
      TypeDefinition:integer;
      Kind:integer;
      Value:integer;
      VariableLevel:integer;
      VariableAddress:integer;
      ReferencedParameter:boolean;
      Offset:integer;
      FunctionLevel:integer;
      FunctionAddress:integer;
      LastParameter:integer;
      ReturnAddress:integer;
      Inside:boolean;
     end;

     TType=record
      Size:integer;
      Kind:integer;
      StartIndex:integer;
      EndIndex:integer;
      SubType:integer;
      Fields:integer;
     end;

var CurrentChar:char;
    CurrentColumn:integer;
    CurrentLine:integer;
    CurrentSymbol:integer;
    CurrentIdentifer:TAlfa;
    CurrentNumber:integer;
    CurrentString:array[1:255] of char;
    CurrentStringLength:integer;
    FunctionDeclarationIndex:integer;
    Keywords:array[SymBEGIN..SymPROC] of TAlfa;
    LastOpcode:integer;
    CurrentLevel:integer;
    IsLabeled:boolean;
    SymbolNameList:array[-1..MaximalList] of integer;
    IdentifierPosition:integer;
    TypePosition:integer;
    Identifiers:array[0..MaximalIdentifiers] of TIdent;
    Types:array[1..MaximalTypes] of TType;
    Code:array[0:MaximalCodeSize] of integer;
    CodePosition:integer;
    StackPosition:integer;

function StringCompare(var s1,s2:TAlfa):boolean;
var f:boolean;
    i:integer;
begin
 f:=true;
 i:=1;
 while f and (i<=MaximalAlfa) do begin
  f:=(s1[i]=s2[i]);
  i:=i+1;
 end;
 StringCompare:=f;
end;

procedure StringCopy(var Dest:TAlfa;Src:TAlfa);
begin
 Dest:=Src;
end;

procedure Error(n:integer);
begin
 Write('Error ',n:1,': ');
 case n of
  TokIdent:begin
   Write('Identifier expected');
  end;
  TokNumber:begin
   Write('Number expected');
  end;
  TokStrC:begin
   Write('String expected');
  end;
  TokPlus:begin
   Write('"+" expected');
  end;
  TokMinus:begin
   Write('"-" expected');
  end;
  TokMul:begin
   Write('"*" expected');
  end;
  TokLBracket:begin
   Write('"[" expected');
  end;
  TokRBracket:begin
   Write('"]" expected');
  end;
  TokColon:begin
   Write('":" expected');
  end;
  TokEql:begin
   Write('"=" expected');
  end;
  TokNEq:begin
   Write('"<>" expected');
  end;
  TokLss:begin
   Write('"<" expected');
  end;
  TokLEq:begin
   Write('"<=" expected');
  end;
  TokGtr:begin
   Write('">" expected');
  end;
  TokGEq:begin
   Write('">=" expected');
  end;
  TokLParent:begin
   Write('"(" expected');
  end;
  TokRParent:begin
   Write('")" expected');
  end;
  TokComma:begin
   Write('"," expected');
  end;
  TokSemi:begin
   Write('";" expected');
  end;
  TokPeriod:begin
   Write('"." expected');
  end;
  TokAssign:begin
   Write('":=" expected');
  end;
  SymBEGIN:begin
   Write('"begin" expected');
  end;
  SymEND:begin
   Write('"end" expected');
  end;
  SymIF:begin
   Write('"if" expected');
  end;
  SymTHEN:begin
   Write('"then" expected');
  end;
  SymELSE:begin
   Write('"else" expected');
  end;
  SymWHILE:begin
   Write('"else" expected');
  end;
  SymDO:begin
   Write('"do" expected');
  end;
  SymCASE:begin
   Write('"case" expected');
  end;
  SymREPEAT:begin
   Write('"repeat" expected');
  end;
  SymUNTIL:begin
   Write('"until" expected');
  end;
  SymFOR:begin
   Write('"for" expected');
  end;
  SymTO:begin
   Write('"to" expected');
  end;
  SymDOWNTO:begin
   Write('"downto" expected');
  end;
  SymNOT:begin
   Write('"not" expected');
  end;
  SymDIV:begin
   Write('"div" expected');
  end;
  SymMOD:begin
   Write('"mod" expected');
  end;
  SymAND:begin
   Write('"and" expected');
  end;
  SymOR:begin
   Write('"or" expected');
  end;
  SymCONST:begin
   Write('"const" expected');
  end;
  SymVAR:begin
   Write('"var" expected');
  end;
  SymTYPE:begin
   Write('"type" expected');
  end;
  SymARRAY:begin
   Write('"array" expected');
  end;
  SymOF:begin
   Write('"of" expected');
  end;
  SymPACKED:begin
   Write('"packed" expected');
  end;
  SymRECORD:begin
   Write('"record" expected');
  end;
  SymPROGRAM:begin
   Write('"program" expected');
  end;
  SymFORWARD:begin
   Write('"forward" expected');
  end;
  SymHALT:begin
   Write('"halt" expected');
  end;
  SymFUNC:begin
   Write('"function" expected');
  end;
  SymPROC:begin
   Write('"procedure" expected');
  end;
  100:begin
   Write('String literal must be closed');
  end;
  101:begin
   Write('String is empty');
  end;
  102:begin
   Write('Bad char');
  end;
  103:begin
   Write('Too many identifiers');
  end;
  104:begin
   Write('Duplicate identifier');
  end;
  105:begin
   Write('Duplicate procedure/function');
  end;
  106:begin
   Write('Unknown identifiers');
  end;
  107:begin
   Write('Invalid type');
  end;
  108:begin
   Write('Record type expected');
  end;
  109:begin
   Write('Unknown field');
  end;
  110:begin
   Write('Array type expected');
  end;
  111:begin
   Write('Non-writeable type');
  end;
  112:begin
   Write('Non-readable type');
  end;
  113:begin
   Write('Too many argumnts');
  end;
  114:begin
   Write('Passing string to var argument isn''t allowed');
  end;
  115:begin
   Write('Passing string to non-array argument isn''t allowed');
  end;
  116:begin
   Write('Passing string to non-char-array argument isn''t allowed');
  end;
  117:begin
   Write('Passing string to wrong sized char-array argument isn''t allowed');
  end;
  118:begin
   Write('Too few argumnts');
  end;
  119:begin
   Write('Procedure calls inside a expression aren''t allowed');
  end;
  120:begin
   Write('Type inside a expression isn''t allowed');
  end;
  121:begin
   Write('Expression expected');
  end;
  122:begin
   Write('Illegal assigning to function');
  end;
  123:begin
   Write('Illegal assigning to constant or type');
  end;
  124:begin
   Write('Case expression must be constant');
  end;
  125:begin
   Write('Case expression expected');
  end;
  126:begin
   Write('":" expected');
  end;
  127:begin
   Write('Variable after "FOR" expected');
  end;
  128:begin
   Write('Incorrect iterator type');
  end;
  129:begin
   Write('"TO" or "DOWNTO" expected');
  end;
  130:begin
   Write('Constant expected');
  end;
  131:begin
   Write('Identifier or number literal expected');
  end;
  132:begin
   Write('First index of array must be less or equal then last');
  end;
  133:begin
   Write('Type expected');
  end;
  134:begin
   Write('Too many types');
  end;
  135:begin
   Write('Too many nested records');
  end;
  136:begin
   Write('Too many nested procedures');
  end;
  137:begin
   Write('Invalid function return type');
  end;
  138:begin
   Write('Too many arguments then at forward declaration');
  end;
  139:begin
   Write('Argument name doesn''t match forward declaration');
  end;
  140:begin
   Write('Argument type doesn''t match forward declaration');
  end;
  141:begin
   Write('Argument var doesn''t match forward declaration');
  end;
  142:begin
   Write('Too less arguments then at forward declaration');
  end;
  143:begin
   Write('Already forward declared');
  end;
  144:begin
   Write('No definition for forward declared');
  end;
  145:begin
   Write('Internal negative retn');
  end;
  146:begin
   Write('Binary is too big');
  end;
  147:begin
   Write('String is too long');
  end;
  148:begin
   Write('Too many cases');
  end;
  149:begin
   Write('Too large code');
  end;
  150:begin
   Write('Incompatible types');
  end;
 end;
 WriteLn(' at line ',CurrentLine:1,' at column ',CurrentColumn:1);
 Halt;
end;

procedure ReadChar;
begin
 if not EOF then begin
  read(CurrentChar);
  CurrentColumn:=CurrentColumn+1;
  if CurrentChar=#10 then begin
   CurrentLine:=CurrentLine+1;
   CurrentColumn:=0;
  end;
 end else begin
  CurrentChar:=#0;
 end;
end;

function ReadNumber:integer;
var Num:integer;
begin
 Num:=0;
 if ('0'<=CurrentChar) and (CurrentChar<='9') then begin
  while ('0'<=CurrentChar) and (CurrentChar<='9') do begin
   Num:=(Num*10)+(ord(CurrentChar)-ord('0'));
   ReadChar;
  end;
 end else if CurrentChar='$' then begin
  ReadChar;
  while (('0'<=CurrentChar) and (CurrentChar<='9')) or
        (('a'<=CurrentChar) and (CurrentChar<='f')) or
        (('A'<=CurrentChar) and (CurrentChar<='F')) do begin
   if ('0'<=CurrentChar) and (CurrentChar<='9') then begin
    Num:=(Num*16)+(ord(CurrentChar)-ord('0'));
   end else if ('a'<=CurrentChar) and (CurrentChar<='f') then begin
    Num:=(Num*16)+(ord(CurrentChar)-ord('a')+10);
   end else if ('A'<=CurrentChar) and (CurrentChar<='F') then begin
    Num:=(Num*16)+(ord(CurrentChar)-ord('A')+10);
   end;
   ReadChar;
  end;
 end;
 ReadNumber:=Num;
end;

procedure GetSymbol;
var k,s:integer;
    StrEnd,InStr:boolean;
    LastChar:char;
begin
 while (CurrentChar>#0) and (CurrentChar<=' ') do begin
  ReadChar;
 end;
 if (('a'<=CurrentChar) and (CurrentChar<='z')) or (('A'<=CurrentChar) and (CurrentChar<='Z')) then begin
  k:=0;
  while ((('a'<=CurrentChar) and (CurrentChar<='z')) or
         (('A'<=CurrentChar) and (CurrentChar<='Z')) or
         (('0'<=CurrentChar) and (CurrentChar<='9'))) or
        (CurrentChar='_') do begin
   if k<>MaximalAlfa then begin
    k:=k+1;
    if ('a'<=CurrentChar) and (CurrentChar<='z') then begin
     CurrentChar:=chr(ord(CurrentChar)-32);
    end;
    CurrentIdentifer[k]:=CurrentChar;
   end;
   ReadChar;
  end;
  while k<>MaximalAlfa do begin
   k:=k+1;
   CurrentIdentifer[k]:=' ';
  end;
  CurrentSymbol:=TokIdent;
  s:=SymBEGIN;
  while s<=SymPROC do begin
   if StringCompare(Keywords[s],CurrentIdentifer) then begin
    CurrentSymbol:=s;
   end;
   s:=s+1;
  end;
 end else if (('0'<=CurrentChar) and (CurrentChar<='9')) or (CurrentChar='$') then begin
  CurrentSymbol:=TokNumber;
  CurrentNumber:=ReadNumber;
 end else if CurrentChar=':' then begin
  ReadChar;
  if CurrentChar='=' then begin
   ReadChar;
   CurrentSymbol:=TokAssign;
  end else begin
   CurrentSymbol:=TokColon;
  end;
 end else if CurrentChar='>' then begin
  ReadChar;
  if CurrentChar='=' then begin
   ReadChar;
   CurrentSymbol:=TokGEq;
  end else begin
   CurrentSymbol:=TokGtr;
  end;
 end else if CurrentChar='<' then begin
  ReadChar;
  if CurrentChar='=' then begin
   ReadChar;
   CurrentSymbol:=TokLEq;
  end else if CurrentChar='>' then begin
   ReadChar;
   CurrentSymbol:=TokNEq;
  end else begin
   CurrentSymbol:=TokLss;
  end;
 end else if CurrentChar='.' then begin
  ReadChar;
  if CurrentChar='.' then begin
   ReadChar;
   CurrentSymbol:=TokColon;
  end else begin
   CurrentSymbol:=TokPeriod
  end;
 end else if (CurrentChar='''') or (CurrentChar='#') then begin
  CurrentStringLength:=0;
  StrEnd:=false;
  InStr:=false;
  CurrentSymbol:=TokStrC;
  while not StrEnd do begin
   if InStr then begin
    if CurrentChar='''' then begin
     ReadChar;
     if CurrentChar='''' then begin
      if CurrentStringLength=MaximalStringLength then begin
       Error(147);
      end;
      CurrentStringLength:=CurrentStringLength+1;
      CurrentString[CurrentStringLength]:=CurrentChar;
      ReadChar;
     end else begin
      InStr:=false;
     end;
    end else if (CurrentChar=#13) or (CurrentChar=#10) then begin
     Error(100);
     StrEnd:=true;
    end else begin
      if CurrentStringLength=MaximalStringLength then begin
       Error(147);
      end;
     CurrentStringLength:=CurrentStringLength+1;
     CurrentString[CurrentStringLength]:=CurrentChar;
     ReadChar;
    end;
   end else begin
    if CurrentChar='''' then begin
     InStr:=true;
     ReadChar;
    end else if CurrentChar='#' then begin
     ReadChar;
     if CurrentStringLength=MaximalStringLength then begin
      Error(147);
     end;
     CurrentStringLength:=CurrentStringLength+1;
     CurrentString[CurrentStringLength]:=chr(ReadNumber);
    end else begin
     StrEnd:=true;
    end;
   end;
  end;
  if CurrentStringLength=0 then begin
   Error(101);
  end;
 end else if CurrentChar='+' then begin
  ReadChar;
  CurrentSymbol:=TokPlus;
 end else if CurrentChar='-' then begin
  ReadChar;
  CurrentSymbol:=TokMinus;
 end else if CurrentChar='*' then begin
  ReadChar;
  CurrentSymbol:=TokMul;
 end else if CurrentChar='(' then begin
  ReadChar;
  if CurrentChar='*' then begin
   ReadChar;
   LastChar:='-';
   while (CurrentChar<>#0) and not ((CurrentChar=')') and (LastChar='*')) do begin
    LastChar:=CurrentChar;
    ReadChar;
   end;
   ReadChar;
   GetSymbol;
  end else begin
   CurrentSymbol:=TokLParent;
  end;
 end else if CurrentChar=')' then begin
  ReadChar;
  CurrentSymbol:=TokRParent;
 end else if CurrentChar='[' then begin
  ReadChar;
  CurrentSymbol:=TokLBracket;
 end else if CurrentChar=']' then begin
  ReadChar;
  CurrentSymbol:=TokRBracket;
 end else if CurrentChar='=' then begin
  ReadChar;
  CurrentSymbol:=TokEql;
 end else if CurrentChar=',' then begin
  ReadChar;
  CurrentSymbol:=TokComma;
 end else if CurrentChar=';' then begin
  ReadChar;
  CurrentSymbol:=TokSemi;
 end else if CurrentChar='{' then begin
  while (CurrentChar<>'}') and (CurrentChar<>#0) do begin
   ReadChar;
  end;
  ReadChar;
  GetSymbol;
 end else if CurrentChar='/' then begin
  ReadChar;
  if CurrentChar='/' then begin
   repeat
    ReadChar;
   until (CurrentChar=#10) or (CurrentChar=#0);
   GetSymbol;
  end else begin
   Error(102);
  end;
 end else begin
  Error(102);
 end;
end;

procedure Check(s:integer);
begin
 if CurrentSymbol<>s then begin
  Error(s);
 end;
end;

procedure Expect(s:integer);
begin
 Check(s);
 GetSymbol;
end;

procedure EnterSymbol(CurrentIdentifer:TAlfa;k,t:integer);
var j:integer;
begin
 if IdentifierPosition=MaximalIdentifiers then begin
  Error(103);
 end;
 IdentifierPosition:=IdentifierPosition+1;
 Identifiers[0].Name:=CurrentIdentifer;
 j:=SymbolNameList[CurrentLevel];
 while not StringCompare(Identifiers[j].Name,CurrentIdentifer) do begin
  j:=Identifiers[j].Link;
 end;
 if j<>0 then begin
  if Identifiers[j].Kind<>IdFUNC then begin
   Error(104);
  end;
  if (Code[Identifiers[j].FunctionAddress]<>OPJmp) or (Code[Identifiers[j].FunctionAddress+1]>0) then begin
   Error(105);
  end;
  Identifiers[j].Name[1]:='$';
  Code[Identifiers[j].FunctionAddress+1]:=CodePosition;
  FunctionDeclarationIndex:=j;
 end;
 Identifiers[IdentifierPosition].Name:=CurrentIdentifer;
 Identifiers[IdentifierPosition].Link:=SymbolNameList[CurrentLevel];
 Identifiers[IdentifierPosition].TypeDefinition:=t;
 Identifiers[IdentifierPosition].Kind:=k;
 SymbolNameList[CurrentLevel]:=IdentifierPosition;
end;

function Position:integer;
var i,j:integer;
begin
 Identifiers[0].Name:=CurrentIdentifer;
 i:=CurrentLevel;
 repeat
  j:=SymbolNameList[i];
  while not StringCompare(Identifiers[j].Name,CurrentIdentifer) do begin
   j:=Identifiers[j].Link;
  end;
  i:=i-1;
 until (i<-1) or (j<>0);
 if j=0 then begin
  Error(106);
 end;
 Position:=j;
end;


procedure EmitCode(Value:integer);
begin
 if CodePosition>MaximalCodeSize then begin
  Error(149);
 end;
 Code[CodePosition]:=Value;
 CodePosition:=CodePosition+1;
end;

procedure EmitOpcode(Opcode,a:integer);
begin
 case Opcode of
  OPDupl,OPEOF,OPEOL,OPLdC,OPLdA,OPLdLA,OPLdL,OPLdG:begin
   StackPosition:=StackPosition-4;
  end;
  OPNeg,OPDiv2,OPRem2,OPSwap,OPLoad,OPHalt,OPWrL,OPRdL,OpAddC,OPMulC,
  OPJmp,OPCall,OPExit:begin
  end;
  OPAdd,OPMul,OPDivD,OPRemD,OPEqlI,OPNEqI,OPLssI,OPLeqI,OPGtrI,OPGEqI,OPAndB,
  OPOrB,OPWrC,OPRdI,OPRdC,OPStL,OPStG,OPJZ:begin
   StackPosition:=StackPosition+4;
  end;
  OPStore,OPWrI,OPMove:begin
   StackPosition:=StackPosition+8;
  end;
  OPCopy:begin
   StackPosition:=StackPosition-(a-4);
  end;
  OPAdjS:begin
   StackPosition:=StackPosition+a;
  end;
 end;
 if not ((((Opcode=OPAddC) or (Opcode=OPAdjS)) and (a=0)) or ((Opcode=OPMulC) and (a=1))) then begin
  if IsLabeled then begin
   Code[CodePosition]:=Opcode;
   CodePosition:=CodePosition+1;
   if Opcode>=OPLdC then begin
    Code[CodePosition]:=a;
    CodePosition:=CodePosition+1;
   end;
   IsLabeled:=false;
  end else if (LastOpcode=OPLdC) and (Opcode=OPAdd) then begin
   Code[CodePosition-2]:=OPAddC;
  end else if (LastOpcode=OPLdC) and (Opcode=OPMul) then begin
   Code[CodePosition-2]:=OPMulC;
  end else if (LastOpcode=OPLdC) and (Opcode=OPNeg) then begin
   Code[CodePosition-1]:=-Code[CodePosition-1];
   Opcode:=LastOpcode;
  end else if (LastOpcode=OPLdC) and (Code[CodePosition-1]=2) and (Opcode=OPDivD) then begin
   Code[CodePosition-2]:=OPDiv2;
   CodePosition:=CodePosition-1;
  end else if (LastOpcode=OPLdC) and (Code[CodePosition-1]=2) and (Opcode=OPRemD) then begin
   Code[CodePosition-2]:=OPRem2;
   CodePosition:=CodePosition-1;
  end else if (LastOpcode=OPLdA) and (Opcode=OPStore) then begin
   Code[CodePosition-2]:=OPStG;
  end else if (LastOpcode=OPLdA) and (Opcode=OPLoad) then begin
   Code[CodePosition-2]:=OPLdG;
  end else if (LastOpcode=OPLdLA) and (Opcode=OPStore) then begin
   Code[CodePosition-2]:=OPStL;
  end else if (LastOpcode=OPLdLA) and (Opcode=OPLoad) then begin
   Code[CodePosition-2]:=OPLdL;
  end else begin
   EmitCode(Opcode);
   if Opcode>=OPLdC then begin
    EmitCode(a);
   end;
  end;
  LastOpcode:=Opcode;
 end;
end;

procedure EmitOpcode2(Opcode:integer);
begin
 EmitOpcode(Opcode,0);
end;

function CodeLabel:integer;
begin
 CodeLabel:=CodePosition;
 IsLabeled:=true;
end;

procedure EmitAddress(Level,Address:integer);
begin
 if Level=0 then begin
  EmitOpcode(OPLdA,Address);
 end else if Level=CurrentLevel then begin
  EmitOpcode(OPLdLA,Address-StackPosition);
 end else begin
  EmitOpcode(OPLdL,-StackPosition);
  while Level+1<>CurrentLevel do begin
   EmitOpcode2(OPLoad);
   Level:=Level+1;
  end;
  EmitOpcode(OPAddC,Address);
 end;
end;

procedure EmitAddressVar(IdentifierIndex:integer);
begin
 EmitAddress(Identifiers[IdentifierIndex].VariableLevel,Identifiers[IdentifierIndex].VariableAddress);
 if Identifiers[IdentifierIndex].ReferencedParameter then begin
  EmitOpcode2(OPLoad);
 end;
end;

procedure MustBe(x,y:integer);
begin
 if x<>y then begin
  if (Types[x].Kind=KindARRAY) and
     (Types[y].Kind=KindARRAY) and
     (Types[x].StartIndex=Types[y].StartIndex) and
     (Types[x].EndIndex=Types[y].EndIndex) then begin
   MustBe(Types[x].SubType,Types[y].SubType);
  end else begin
   Error(107);
  end;
 end;
end;

procedure Expression(var x:integer); forward;

procedure Selector(var t,IdentifierIndex:integer);
var j,x:integer;
begin
 t:=Identifiers[IdentifierIndex].TypeDefinition;
 GetSymbol;
 if (CurrentSymbol=TokPeriod) or (CurrentSymbol=TokLBracket) then begin
  EmitAddressVar(IdentifierIndex);
  IdentifierIndex:=0;
  while (CurrentSymbol=TokPeriod) or (CurrentSymbol=TokLBracket) do begin
   case CurrentSymbol of
    TokPeriod:begin
     if Types[t].Kind<>KindRECORD then begin
      Error(108);
     end;
     GetSymbol;
     Check(TokIdent);
     j:=Types[t].Fields;
     Identifiers[0].Name:=CurrentIdentifer;
     while not StringCompare(Identifiers[j].Name,CurrentIdentifer) do begin
      j:=Identifiers[j].Link;
     end;
     if j=0 then begin
      Error(109);
     end;
     {this opcode also being used for offsetting in structurese}
     {so we double it on x64}
     EmitOpcode(OPAddC,Identifiers[j].Offset * 2);
     t:=Identifiers[j].TypeDefinition;
     GetSymbol;
    end;
    TokLBracket:begin
     repeat
      if Types[t].Kind<>KindARRAY then begin
       Error(110);
      end;
      GetSymbol;
      Expression(x);
      MustBe(TypeINT,x);
      EmitOpcode(OPAddC,-Types[t].StartIndex);
      t:=Types[t].SubType;
      {in arrays mul is used to get offsets of elemnts in stack}
      {so value needs to be doubled on x64}
      EmitOpcode(OPMulC,Types[t].Size * 2);
      EmitOpcode2(OPAdd);
     until CurrentSymbol<>TokComma;
     Expect(TokRBracket)
    end;
   end;
  end;
 end;
end;

procedure VarPar(var t:integer);
var j:integer;
begin
 Check(TokIdent);
 j:=Position;
 Selector(t,j);
 if j<>0 then begin
  EmitAddressVar(j);
 end;
end;

procedure InternalFunction(n:integer);
var x:integer;
begin
 case n of
  FunCHR:begin
   Expect(TokLParent);
   Expression(x);
   MustBe(TypeINT,x);
   Expect(TokRParent)
  end;
  FunORD:begin
   Expect(TokLParent);
   Expression(x);
   if x<>TypeBOOL then begin
    MustBe(TypeCHAR,x);
   end;
   Expect(TokRParent);
  end;
  FunWRITE,FunWRITELN:begin
   if n=FunWRITE then begin
    Check(TokLParent);
   end;
   if CurrentSymbol=TokLParent then begin
    repeat
     GetSymbol;
     if CurrentSymbol=TokStrC then begin
      x:=1;
      while x<=CurrentStringLength do begin
       EmitOpcode(OPLdC,ord(CurrentString[x])); {OPLdC == push byte/dword value}
       EmitOpcode2(OPWrC);                      {writeChar(arg = top_of_stack)}
       x:=x+1;
      end;
      GetSymbol;
     end else begin
      Expression(x);
      if CurrentSymbol=TokColon then begin
       MustBe(TypeINT,x);
       GetSymbol;
       Expression(x);
       MustBe(TypeINT,x);
       EmitOpcode2(OPWrI);
      end else if x=TypeINT then begin
       EmitOpcode(OPLdC,1);
       EmitOpcode2(OPWrI);
      end else if x=TypeCHAR then begin
       EmitOpcode2(OPWrC);
      end else begin
       Error(111);
      end;
     end;
    until CurrentSymbol<>TokComma;
    Expect(TokRParent)
   end;
   if n=FunWRITELN then begin
    EmitOpcode2(OPWrL);
   end;
  end;
  FunREAD,FunREADLN:begin
   if n=FunREAD then begin
    Check(TokLParent);
   end;
   if CurrentSymbol=TokLParent then begin
    repeat
     GetSymbol;
     VarPar(x);
     if x=TypeINT then begin
      EmitOpcode2(OPRdI); {ReadInteger is not used for bootsrtapping}
     end else if x=TypeCHAR then begin
      EmitOpcode2(OPRdC);
     end else begin
      Error(112);
     end;
    until CurrentSymbol<>TokComma;
    Expect(TokRParent);
   end;
   if n=FunREADLN then begin
    EmitOpcode2(OPRdL);
   end;
  end;
  FunEOF:begin
   EmitOpcode2(OPEOF);
  end;
  FunEOFLN:begin
   EmitOpcode2(OPEOL);
  end;
 end;
end;

procedure FunctionCall(i:integer);
var OldStackPosition,p,x:integer;
begin
 GetSymbol;
 if Identifiers[i].FunctionLevel<0 then begin
  InternalFunction(Identifiers[i].FunctionAddress);
 end else begin
  if Identifiers[i].TypeDefinition<>0 then begin
   EmitOpcode(OPLdC,0);
  end;
  p:=i;
  OldStackPosition:=StackPosition;
  if CurrentSymbol=TokLParent then begin
   repeat
    GetSymbol;
    if p=Identifiers[i].LastParameter then begin
     Error(113);
    end;
    p:=p+1;
    if Identifiers[p].ReferencedParameter then begin
     VarPar(x);
    end else begin
     Expression(x);
     if Types[x].Kind<>KindSIMPLE then begin
      EmitOpcode(OPCopy,Types[x].Size);
     end;
    end;
    if x=TypeSTR then begin
     if Identifiers[p].ReferencedParameter then begin
      Error(114);
     end;
     if Types[Identifiers[p].TypeDefinition].Kind<>KindARRAY then begin
      Error(115);
     end;
     if Types[Identifiers[p].TypeDefinition].SubType<>TypeCHAR then begin
      Error(116);
     end;
     if ((Types[Identifiers[p].TypeDefinition].EndIndex
          -Types[Identifiers[p].TypeDefinition].StartIndex)+1)
        <>CurrentStringLength then begin
      Error(117);
     end;
    end else begin
     MustBe(Identifiers[p].TypeDefinition,x);
    end;
   until CurrentSymbol<>TokComma;
   Expect(TokRParent);
  end;
  if p<>Identifiers[i].LastParameter then begin
   Error(118);
  end;
  if Identifiers[i].FunctionLevel<>0 then begin
   EmitAddress(Identifiers[i].FunctionLevel,0);
  end;
  EmitOpcode(OPCall,Identifiers[i].FunctionAddress);
  StackPosition:=OldStackPosition;
 end;
end;

procedure Factor(var t:integer);
var i:integer;
begin
 if CurrentSymbol=TokIdent then begin
  i:=Position;
  t:=Identifiers[i].TypeDefinition;
  case Identifiers[i].Kind of
   IdCONST:begin
    GetSymbol;
    EmitOpcode(OPLdC,Identifiers[i].Value);
   end;
   IdVAR:begin
    Selector(t,i);
    if i<>0 then begin
     EmitAddressVar(i);
    end;
    if Types[t].Kind=KindSIMPLE then begin
     EmitOpcode2(OPLoad);
    end;
   end;
   IdFUNC:begin
    if t=0 then begin
     Error(119);
    end else begin
     FunctionCall(i);
    end;
   end;
   IdTYPE:begin
    Error(120);
   end;
  end;
 end else if CurrentSymbol=TokNumber then begin
  EmitOpcode(OPLdC,CurrentNumber);
  t:=TypeINT;
  GetSymbol;
 end  else if CurrentSymbol=TokStrC then begin
  i:=CurrentStringLength;
  while i>=1 do begin
   EmitOpcode(OPLdC,ord(CurrentString[i]));
   i:=i-1;
  end;
  t:=TypeCHAR;
  if CurrentStringLength<>1 then begin
   t:=TypeSTR;
  end;
  GetSymbol;
 end else if CurrentSymbol=TokLParent then begin
  GetSymbol;
  Expression(t);
  Expect(TokRParent);
 end else if CurrentSymbol=SymNOT then begin
  GetSymbol;
  Factor(t);
  MustBe(TypeBOOL,t);
  EmitOpcode2(OPNeg);
  EmitOpcode(OPAddC,1);
 end else begin
  Error(121);
 end;
end;

procedure Term(var x:integer);
var y:integer;
begin
 Factor(x);
 while (CurrentSymbol=SymAND) or (CurrentSymbol=TokMul) or (CurrentSymbol=SymDIV) or (CurrentSymbol=SymMOD) do begin
  if CurrentSymbol=SymAND then begin
   MustBe(TypeBOOL,x);
  end else begin
   MustBe(TypeINT,x);
  end;
  case CurrentSymbol of
   TokMul:begin
    GetSymbol;
    Factor(y);
    EmitOpcode2(OPMul);
   end;
   SymDIV:begin
    GetSymbol;
    Factor(y);
    EmitOpcode2(OPDivD);
   end;
   SymMOD:begin
    GetSymbol;
    Factor(y);
    EmitOpcode2(OPRemD);
   end;
   SymAND:begin
    GetSymbol;
    Factor(y);
    EmitOpcode2(OPAndB);
   end;
  end;
  MustBe(x,y);
 end;
end;

procedure SimpleExpression(var x:integer);
var y:integer;
begin
 if CurrentSymbol=TokPlus then begin
  GetSymbol;
  Term(x);
  MustBe(TypeINT,x);
 end else if CurrentSymbol=TokMinus then begin
  GetSymbol;
  Term(x);
  MustBe(TypeINT,x);
  EmitOpcode2(OPNeg);
 end else begin
  Term(x);
 end;
 while (CurrentSymbol=SymOR) or (CurrentSymbol=TokPlus) or (CurrentSymbol=TokMinus) do begin
  if CurrentSymbol=SymOR then begin
   MustBe(TypeBOOL,x);
  end else begin
   MustBe(TypeINT,x);
  end;
  case CurrentSymbol of
   TokPlus:begin
    GetSymbol;
    Term(y);
    EmitOpcode2(OPAdd);
   end;
   TokMinus:begin
    GetSymbol;
    Term(y);
    EmitOpcode2(OPNeg);
    EmitOpcode2(OPAdd);
   end;
   SymOR:begin
    GetSymbol;
    Term(y);
    EmitOpcode2(OPOrB);
   end;
  end;
  MustBe(x,y);
 end;
end;

procedure Expression(var x:integer);
var o,y:integer;
begin
 SimpleExpression(x);
 if (CurrentSymbol=TokEql) or (CurrentSymbol=TokNEq) or
    (CurrentSymbol=TokLss) or (CurrentSymbol=TokLEq) or
    (CurrentSymbol=TokGtr) or (CurrentSymbol=TokGEq) then begin
  if (x=TypeSTR) or (Types[x].Kind<>KindSIMPLE) then begin
   Error(150);
  end;
  o:=CurrentSymbol;
  GetSymbol;
  SimpleExpression(y);
  MustBe(x,y);
  case o of
   TokEql:begin
    EmitOpcode2(OPEqlI);
   end;
   TokNEq:begin
    EmitOpcode2(OPNEqI);
   end;
   TokLss:begin
    EmitOpcode2(OPLssI);
   end;
   TokLEq:begin
    EmitOpcode2(OPLeqI);
   end;
   TokGtr:begin
    EmitOpcode2(OPGtrI);
   end;
   TokGEq:begin
    EmitOpcode2(OPGEqI);
   end;
  end;
  x:=TypeBOOL;
 end;
end;

procedure Statement;
var L:array[1:MaximalCases] of integer;
    m,n,i,j,t,x,r,OldStackPosition:integer;
begin
 if CurrentSymbol=TokIdent then begin
  i:=Position;
  case Identifiers[i].Kind of
   IdVAR:begin
    Selector(t,i);
    Expect(TokAssign);
    Expression(x);
    MustBe(t,x);
    if i=0 then begin
     EmitOpcode2(OPSwap);
    end else begin
     EmitAddressVar(i);
    end;
    if Types[t].Kind=KindSIMPLE then begin
     EmitOpcode2(OPStore);
    end else begin
     EmitOpcode(OPMove,Types[t].Size);
    end;
   end;
   IdFUNC:begin
    if Identifiers[i].TypeDefinition=0 then begin
     FunctionCall(i);
    end else begin
     if not Identifiers[i].Inside then begin
      Error(122);
     end;
     GetSymbol;
     Expect(TokAssign);
     Expression(x);
     MustBe(Identifiers[i].TypeDefinition,x);
     EmitAddress(Identifiers[i].FunctionLevel+1,Identifiers[i].ReturnAddress);
     EmitOpcode2(OPStore);
    end;
   end;
   IdCONST,IdFIELD,IdTYPE:Error(123);
  end;
 end else if CurrentSymbol=SymIF then begin
  GetSymbol;
  Expression(t);
  MustBe(TypeBOOL,t);
  Expect(SymTHEN);
  i:=CodeLabel;
  EmitOpcode(OPJZ,0);
  Statement;
  if CurrentSymbol=SymELSE then begin
   GetSymbol;
   j:=CodeLabel;
   EmitOpcode(OPJmp,0);
   Code[i+1]:=CodeLabel;
   i:=j;
   Statement;
  end;
  Code[i+1]:=CodeLabel;
 end else if CurrentSymbol=SymCASE then begin
  GetSymbol;
  Expression(t);
  MustBe(TypeINT,t);
  Expect(SymOF);
  j:=0;
  m:=0;
  repeat
   if j<>0 then begin
    Code[j+1]:=CodeLabel;
   end;
   n:=m;
   repeat
    if n<>m then begin
     GetSymbol;
    end;
    EmitOpcode2(OPDupl);
    if CurrentSymbol=TokIdent then begin
     i:=Position;
     if Identifiers[i].Kind<>IdCONST then begin
      Error(124);
     end;
     EmitOpcode(OPLdC,Identifiers[i].Value);
    end else if CurrentSymbol=TokNumber then begin
     EmitOpcode(OPLdC,CurrentNumber);
    end else if (CurrentSymbol=TokStrC) and (CurrentStringLength=1) then begin
     EmitOpcode(OPLdC,ord(CurrentString[1]));
    end else begin
     Error(125);
    end;
    EmitOpcode2(OPNEqI);
    if n=MaximalCases then begin
     Error(148);
    end;
    n:=n+1;
    L[n]:=CodeLabel;
    EmitOpcode(OPJZ,0);
    GetSymbol;
   until CurrentSymbol<>TokComma;
   if CurrentSymbol<>TokColon then begin
    Error(126);
   end;
   j:=CodeLabel;
   EmitOpcode(OPJmp,0);
   repeat
    Code[L[n]+1]:=CodeLabel;
    n:=n-1;
   until n=m;
   GetSymbol;
   Statement;
   m:=m+1;
   L[m]:=CodeLabel;
   EmitOpcode(OPJmp,0);
   if CurrentSymbol=TokSemi then begin
    GetSymbol;
   end;
  until CurrentSymbol=SymEND;
  Code[j+1]:=CodeLabel;
  repeat
   Code[L[m]+1]:=CodeLabel;
   m:=m-1;
  until m=0;
  EmitOpcode(OPAdjS,4);
  GetSymbol;
 end else if CurrentSymbol=SymFOR then begin
  GetSymbol;
  if CurrentSymbol=TokIdent then begin
   OldStackPosition:=StackPosition;

   i:=Position;
   if Identifiers[i].Kind<>IdVAR then begin
    Error(127);
   end;
   Selector(t,i);
   Expect(TokAssign);
   Expression(x);
   MustBe(t,x);
   if i=0 then begin
    EmitOpcode2(OPSwap);
   end else begin
    EmitAddressVar(i);
   end;
   if Types[t].Kind<>KindSIMPLE then begin
    Error(128);
   end;
   EmitOpcode2(OPStore);

   r:=1;
   if CurrentSymbol=SymTO then begin
    Expect(SymTO);
   end else if CurrentSymbol=sYMdownto then begin
    Expect(SymDOWNTO);
    r:=-1;
   end else begin
    Error(129);
   end;

   j:=CodeLabel;
   if i=0 then begin
    EmitOpcode2(OPSwap);
   end else begin
    EmitAddressVar(i);
   end;
   EmitOpcode2(OPLoad);
   Expression(x);
   MustBe(t,x);
   if r>0 then begin
    EmitOpcode2(OPLeqI);
   end else begin
    EmitOpcode2(OPGeqI);
   end;
   n:=CodeLabel;
   EmitOpcode(OPJZ,0);

   Expect(SymDO);

   Statement;

   if i=0 then begin
    EmitOpcode2(OPSwap);
   end else begin
    EmitAddressVar(i);
   end;
   EmitOpcode2(OPLoad);

   EmitOpcode(OPAddC,r);

   if i=0 then begin
    EmitOpcode2(OPSwap);
   end else begin
    EmitAddressVar(i);
   end;
   EmitOpcode2(OPStore);

   EmitOpcode(OPJmp,j);
   Code[n+1]:=CodeLabel;

   EmitOpcode(OPAdjS,OldStackPosition-StackPosition);

  end else begin
   Expect(TokIdent);
  end;
 end else if CurrentSymbol=SymWHILE then begin
  GetSymbol;
  i:=CodeLabel;
  Expression(t);
  MustBe(TypeBOOL,t);
  Expect(SymDO);
  j:=CodeLabel;
  EmitOpcode(OPJZ,0);
  Statement;
  EmitOpcode(OPJmp,i);
  Code[j+1]:=CodeLabel;
 end else if CurrentSymbol=SymREPEAT then begin
  i:=CodeLabel;
  repeat
   GetSymbol;
   Statement;
  until CurrentSymbol<>tOKsEMI;
  Expect(SymUNTIL);
  Expression(t);
  MustBe(TypeBOOL,t);
  EmitOpcode(OPJZ,i);
 end else if CurrentSymbol=SymBEGIN then begin
  repeat
   GetSymbol;
   Statement;
  until CurrentSymbol<>TokSemi;
  Expect(SymEND);
 end else if CurrentSymbol=SymHALT then begin
  EmitOpcode2(OPHalt);
  GetSymbol;
 end;
end;

procedure Block(L..integer); forward;

procedure Constant(var c,t..integer);
var i,s:integer;
begin
 if (CurrentSymbol=tOKsTRc) and (CurrentStringLength=1) then begin
  c:=ord(CurrentString[1]);
  t:=TypeCHAR;
 end else begin
  if CurrentSymbol=TokPlus then begin
   GetSymbol;
   s:=1;
  end  else if CurrentSymbol=TokMinus then begin
   GetSymbol;
   s:=-1;
  end else begin
   s:=0;
  end;
  if CurrentSymbol=TokIdent then begin
   i:=Position;
   if Identifiers[i].Kind<>IdCONST then begin
    Error(130);
   end;
   c:=Identifiers[i].Value;
   t:=Identifiers[i].TypeDefinition;
  end else if CurrentSymbol=TokNumber then begin
   c:=CurrentNumber;
   t:=TypeINT;
  end else begin
   Error(131);
  end;
  if s<>0 then begin
   MustBe(t,TypeINT);
   c:=c*s;
  end;
 end;
 GetSymbol;
end;

procedure ConstDeclaration;
var a:TAlfa;
    t,c:integer;
begin
 a:=CurrentIdentifer;
 GetSymbol;
 Expect(TokEql);
 Constant(c,t);
 Expect(TokSemi);
 EnterSymbol(A,IdCONST,t);
 Identifiers[IdentifierPosition].Value:=c;
end;

procedure TypeDefinition(var t:integer); forward;

procedure ArrayType(var t..integer);
var x:integer;
begin
 Types[t].Kind:=KindARRAY;
 GetSymbol;
 Constant(Types[t].StartIndex,x);
 MustBe(TypeINT,x);
 Expect(TokColon);
 Constant(Types[t].EndIndex,x);
 MustBe(TypeINT,x);
 if Types[t].StartIndex>Types[t].EndIndex then begin
  Error(132);
 end;
 if CurrentSymbol=TokComma then begin
  ArrayType(Types[t].SubType);
 end else begin
  Expect(TokRBracket);
  Expect(SymOF);
  TypeDefinition(Types[t].SubType);
 end;
 Types[t].Size:=(Types[t].EndIndex-Types[t].StartIndex+1)*Types[Types[t].SubType].Size;
end;

procedure TypeDefinition(var t:integer);
var i,j,Size,FieldType:integer;
begin
 if CurrentSymbol=SymPACKED then begin
  GetSymbol;
 end;
 if CurrentSymbol=TokIdent then begin
  i:=Position;
  if Identifiers[i].Kind<>IdTYPE then begin
   Error(133);
  end;
  t:=Identifiers[i].TypeDefinition;
  GetSymbol;
 end else begin
  if TypePosition=MaximalTypes then begin
   Error(134);
  end;
  TypePosition:=TypePosition+1;
  t:=TypePosition;
  if CurrentSymbol=SymARRAY then begin
   GetSymbol;
   Check(TokLBracket);
   ArrayType(t);
  end else begin
   Expect(SymRECORD);
   if CurrentLevel=MaximalList then begin
    Error(135);
   end;
   CurrentLevel:=CurrentLevel+1;
   SymbolNameList[CurrentLevel]:=0;
   Check(TokIdent);
   Size:=0;
   repeat
    EnterSymbol(CurrentIdentifer,IdFIELD,0);
    i:=IdentifierPosition;
    GetSymbol;
    while CurrentSymbol=TokComma do begin
     GetSymbol;
     Check(TokIdent);
     EnterSymbol(CurrentIdentifer,IdFIELD,0);
     GetSymbol;
    end;
    j:=IdentifierPosition;
    Expect(TokColon);
    TypeDefinition(FieldType);
    repeat
     Identifiers[i].TypeDefinition:=FieldType;
     Identifiers[i].Offset:=Size;
     Size:=Size+Types[FieldType].Size;
     i:=i+1;
    until i>j;
    if CurrentSymbol=TokSemi then begin
     GetSymbol;
    end else begin
     Check(SymEND);
    end;
   until CurrentSymbol<>TokIdent;
   Types[t].Size:=Size;
   Types[t].Kind:=KindRECORD;
   Types[t].Fields:=SymbolNameList[CurrentLevel];
   CurrentLevel:=CurrentLevel-1;
   Expect(SymEND);
  end;
 end;
end;

procedure TypeDeclaration;
var a:TAlfa;
    t:integer;
begin
 a:=CurrentIdentifer;
 GetSymbol;
 Expect(TokEql);
 TypeDefinition(t);
 Expect(TokSemi);
 EnterSymbol(a,IdTYPE,t);
end;

procedure VarDeclaration;
var p,q,t:integer;
begin
 EnterSymbol(CurrentIdentifer,IdVAR,0);
 p:=IdentifierPosition;
 GetSymbol;
 while CurrentSymbol=TokComma do begin
  GetSymbol;
  Check(TokIdent);
  EnterSymbol(CurrentIdentifer,IdVAR,0);
  GetSymbol;
 end;
 q:=IdentifierPosition;
 Expect(TokColon);
 TypeDefinition(t);
 Expect(TokSemi);
 repeat
  Identifiers[p].VariableLevel:=CurrentLevel;
  StackPosition:=StackPosition-Types[t].Size;
  Identifiers[p].TypeDefinition:=t;
  Identifiers[p].VariableAddress:=StackPosition;
  Identifiers[p].ReferencedParameter:=false;
  p:=p+1;
 until p>q;
end;

procedure NewParameter(var p,LocalStackPosition:integer);
var r:boolean;
    t:integer;
begin
 if CurrentSymbol=SymVAR then begin
  r:=true;
  GetSymbol;
 end else begin
  r:=false;
 end;
 Check(TokIdent);
 p:=IdentifierPosition;
 EnterSymbol(CurrentIdentifer,IdVAR,0);
 GetSymbol;
 while CurrentSymbol=TokComma do begin
  GetSymbol;
  Check(TokIdent);
  EnterSymbol(CurrentIdentifer,IdVAR,0);
  GetSymbol;
 end;
 Expect(TokColon);
 Check(TokIdent);
 TypeDefinition(t);
 while p<IdentifierPosition do begin
  p:=p+1;
  Identifiers[p].TypeDefinition:=t;
  Identifiers[p].ReferencedParameter:=r;
  if r then begin
   LocalStackPosition:=LocalStackPosition+4;
  end else begin
   LocalStackPosition:=LocalStackPosition+Types[t].Size;
  end;
 end;
end;

procedure FunctionDeclaration(IsFunction:boolean);
var f,p,LocalStackPosition,P1,P2,OldStackPosition:integer;
begin
 GetSymbol;
 Check(TokIdent);
 FunctionDeclarationIndex:=-1;
 EnterSymbol(CurrentIdentifer,IdFUNC,0);
 GetSymbol;
 f:=IdentifierPosition;
 Identifiers[f].FunctionLevel:=CurrentLevel;
 Identifiers[f].FunctionAddress:=CodeLabel;
 EmitOpcode(OPJmp,0);
 if CurrentLevel=MaximalList then begin
  Error(136);
 end;
 CurrentLevel:=CurrentLevel+1;
 SymbolNameList[CurrentLevel]:=0;
 LocalStackPosition:=4;
 OldStackPosition:=StackPosition;
 if CurrentSymbol=TokLParent then begin
  repeat
   GetSymbol;
   NewParameter(p,LocalStackPosition);
  until CurrentSymbol<>TokSemi;
  Expect(TokRParent);
 end;
 if CurrentLevel>1 then begin
  StackPosition:=-4;
 end else begin
  StackPosition:=0;
 end;
 Identifiers[f].ReturnAddress:=LocalStackPosition;
 p:=f;
 while p<IdentifierPosition do begin
  p:=p+1;
  if Identifiers[p].ReferencedParameter then begin
   LocalStackPosition:=LocalStackPosition-4;
  end else begin
   LocalStackPosition:=LocalStackPosition-Types[Identifiers[p].TypeDefinition].Size;
  end;
  Identifiers[p].VariableLevel:=CurrentLevel;
  Identifiers[p].VariableAddress:=LocalStackPosition;
 end;
 if IsFunction then begin
  Expect(TokColon);
  Check(TokIdent);
  TypeDefinition(Identifiers[f].TypeDefinition);
  if Types[Identifiers[f].TypeDefinition].Kind<>KindSIMPLE then begin
   Error(137);
  end;
 end;
 Expect(TokSemi);
 Identifiers[f].LastParameter:=IdentifierPosition;
 if CurrentSymbol<>SymFORWARD then begin
  if FunctionDeclarationIndex>=0 then begin
   P1:=FunctionDeclarationIndex+1;
   P2:=f+1;
   while P1<=Identifiers[FunctionDeclarationIndex].LastParameter do begin
    if P2>Identifiers[f].LastParameter then begin
     Error(138);
    end;
    if not StringCompare(Identifiers[P1].Name,Identifiers[P2].Name) then begin
     Error(139);
    end;
    if Identifiers[P1].TypeDefinition<>Identifiers[P2].TypeDefinition then begin
     Error(140);
    end;
    if Identifiers[P1].ReferencedParameter<>Identifiers[P2].ReferencedParameter then begin
     Error(141);
    end;
    P1:=P1+1;
    P2:=P2+1;
   end;
   if P2<=Identifiers[f].LastParameter then begin
    Error(142);
   end;
  end;
  Identifiers[f].Inside:=true;
  Block(Identifiers[f].FunctionAddress);
  Identifiers[f].Inside:=false;
  EmitOpcode(OPExit,Identifiers[f].ReturnAddress-StackPosition);
 end else begin
  if FunctionDeclarationIndex>=0 then begin 
   Error(143);
  end;
  GetSymbol;
 end;
 CurrentLevel:=CurrentLevel-1;
 StackPosition:=OldStackPosition;
 Expect(TokSemi);
end;



procedure Block(L:integer);
var i,d,OldStackPosition,OldIdentPos:integer;
begin
 OldStackPosition:=StackPosition;
 OldIdentPos:=IdentifierPosition;
 while (CurrentSymbol=SymCONST) or (CurrentSymbol=SymTYPE) or
       (CurrentSymbol=SymVAR) or (CurrentSymbol=SymFUNC) or (CurrentSymbol=SymPROC) do begin
  if CurrentSymbol=SymCONST then begin
   GetSymbol;
   Check(TokIdent);
   while CurrentSymbol=TokIdent do begin
    ConstDeclaration;
   end;
  end else if CurrentSymbol=SymTYPE then begin
   GetSymbol;
   Check(TokIdent);
   while CurrentSymbol=TokIdent do begin
    TypeDeclaration;
   end;
  end else if CurrentSymbol=SymVAR then begin
   GetSymbol;
   Check(TokIdent);
   while CurrentSymbol=TokIdent do begin
    VarDeclaration;
   end;
  end else if (CurrentSymbol=SymFUNC) or (CurrentSymbol=SymPROC) then begin
   FunctionDeclaration(CurrentSymbol=SymFUNC);
  end;
 end;
 if L+1=CodeLabel then begin
  CodePosition:=CodePosition-1;
 end else begin
  Code[L+1]:=CodeLabel;
 end;
 if CurrentLevel=0 then begin
  EmitOpcode(OPAdjS,StackPosition);
 end else begin
  d:=StackPosition-OldStackPosition;
  StackPosition:=OldStackPosition;
  EmitOpcode(OPAdjS,d);
 end;
 Statement;
 if CurrentLevel<>0 then begin
  EmitOpcode(OPAdjS,OldStackPosition-StackPosition);
 end;
 i:=OldIdentPos+1;
 while i<=IdentifierPosition do begin
  if Identifiers[i].Kind=IdFUNC then begin
   if (Code[Identifiers[i].FunctionAddress]=OPJmp) and (Code[Identifiers[i].FunctionAddress+1]=0) then begin
    Error(144);
   end;
  end;
  i:=i+1;
 end;
 IdentifierPosition:=OldIdentPos;
end;

const OutputCodeDataMaximalSize=262144;

var OutputCodeData:array[1:OutputCodeDataMaximalSize] of char;
    OutputCodeDataSize:integer;

procedure EmitChar(c:char);
begin
 OutputCodeDataSize:=OutputCodeDataSize+1;
 if OutputCodeDataSize>OutputCodeDataMaximalSize then begin
  Error(146);
 end;
 OutputCodeData[OutputCodeDataSize]:=c;
end;

procedure EmitByte(B:integer);
begin
 EmitChar(chr(B));
end;

procedure EmitInt16(i..integer);
begin
 if i>=0 then begin
  EmitByte(i mod 256);
  EmitByte((i div 256) mod 256);
 end else begin
  i:=-(i+1);
  EmitByte(255-(i mod 256));
  EmitByte(255-((i div 256) mod 256));
 end;
end;

procedure EmitInt32(i..integer);
begin
 if i>=0 then begin
  EmitByte(i mod 256);
  EmitByte((i div 256) mod 256);
  EmitByte((i div 65536) mod 256);
  EmitByte(i div 16777216);
 end else begin
  i:=-(i+1);
  EmitByte(255-(i mod 256));
  EmitByte(255-((i div 256) mod 256));
  EmitByte(255-((i div 65536) mod 256));
  EmitByte(255-(i div 16777216));
 end;
end;

function OutputCodeGetInt32(o:integer):integer;
begin
 if ord(OutputCodeData[o+3])<$80 then begin
  OutputCodeGetInt32:= ord(OutputCodeData[o])+(ord(OutputCodeData[o+1])*256)
    +(ord(OutputCodeData[o+2])*65536)+(ord(OutputCodeData[o+3])*16777216);
 end else begin
  OutputCodeGetInt32:=-(((255-ord(OutputCodeData[o]))
                         +((255-ord(OutputCodeData[o+1]))*256)
                         +((255-ord(OutputCodeData[o+2]))*65536)
                         +((255-ord(OutputCodeData[o+3]))*16777216))+1);
 end;
end;

procedure OutputCodePutInt32(o, i..integer);
begin
 if i>=0 then begin
  OutputCodeData[o]:=chr(i mod 256);
  OutputCodeData[o+1]:=chr((i div 256) mod 256);
  OutputCodeData[o+2]:=chr((i div 65536) mod 256);
  OutputCodeData[o+3]:=chr(i div 16777216);
 end else begin
  i:=-(i+1);
  OutputCodeData[o]:=chr(255-(i mod 256));
  OutputCodeData[o+1]:=chr(255-((i div 256) mod 256));
  OutputCodeData[o+2]:=chr(255-((i div 65536) mod 256));
  OutputCodeData[o+3]:=chr(255-(i div 16777216));
 end;
end;

procedure WriteOutputCode;
var i:integer;
begin
 for i:=1 to OutputCodeDataSize do begin
  write(OutputCodeData[i]);
 end;
end;

type TOutputCodeString=array[1..20] of char;

procedure OutputCodeString(s:TOutputCodeString);
var i:integer;
begin
 for i:=1 to 20 do begin
  EmitChar(s[i]);
 end;
end;

procedure OutputCodeChar(s: char);
var i:integer;
begin
  EmitChar(s);
end;

procedure EmitStubCode;
begin
    OutputCodeDataSize := 0;
    OutputCodeString(#207#250#237#254#12#0#0#1#0#0#0#0#2#0#0#0#16#0#0#0);
    OutputCodeString(#64#3#0#0#133#0#32#0#0#0#0#0#25#0#0#0#72#0#0#0);
    OutputCodeString(#95#95#80#65#71#69#90#69#82#79#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#1#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#25#0#0#0#152#0#0#0#95#95#84#69#88#84#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#1#0#0#0#0#64#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#64#0#0#0#0#0#0);
    OutputCodeString(#5#0#0#0#5#0#0#0#1#0#0#0#0#0#0#0#95#95#116#101);
    OutputCodeString(#120#116#0#0#0#0#0#0#0#0#0#0#95#95#84#69#88#84#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#132#60#0#0#1#0#0#0#124#3#0#0);
    OutputCodeString(#0#0#0#0#132#60#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#4#0#128#0#0#0#0#0#0#0#0#0#0#0#0#25#0#0#0);
    OutputCodeString(#152#0#0#0#95#95#68#65#84#65#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#64#0#0#1#0#0#0#0#64#0#0#0#0#0#0#0#64#0#0);
    OutputCodeString(#0#0#0#0#0#64#0#0#0#0#0#0#3#0#0#0#3#0#0#0);
    OutputCodeString(#1#0#0#0#0#0#0#0#95#95#100#97#116#97#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#95#95#68#65#84#65#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#64#0#0#1#0#0#0#168#0#0#0#0#0#0#0#0#64#0#0);
    OutputCodeString(#4#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#25#0#0#0#72#0#0#0#95#95#76#73);
    OutputCodeString(#78#75#69#68#73#84#0#0#0#0#0#0#0#128#0#0#1#0#0#0);
    OutputCodeString(#0#64#0#0#0#0#0#0#0#128#0#0#0#0#0#0#104#6#0#0);
    OutputCodeString(#0#0#0#0#1#0#0#0#1#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#34#0#0#128#48#0#0#0#0#128#0#0#8#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#8#128#0#0#48#0#0#0#2#0#0#0#24#0#0#0#88#128#0#0);
    OutputCodeString(#35#0#0#0#136#130#0#0#72#2#0#0#11#0#0#0#80#0#0#0);
    OutputCodeString(#0#0#0#0#32#0#0#0#32#0#0#0#2#0#0#0#34#0#0#0);
    OutputCodeString(#1#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#14#0#0#0#32#0#0#0);
    OutputCodeString(#12#0#0#0#47#117#115#114#47#108#105#98#47#100#121#108#100#0#0#0);
    OutputCodeString(#0#0#0#0#27#0#0#0#24#0#0#0#66#208#182#181#51#50#54#234);
    OutputCodeString(#181#121#66#42#244#20#176#9#50#0#0#0#32#0#0#0#1#0#0#0);
    OutputCodeString(#0#0#15#0#0#0#0#0#1#0#0#0#3#0#0#0#3#7#91#4);
    OutputCodeString(#42#0#0#0#16#0#0#0#0#0#0#0#0#0#0#0#40#0#0#128);
    OutputCodeString(#24#0#0#0#132#60#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#12#0#0#0#56#0#0#0#24#0#0#0#2#0#0#0#0#0#71#5);
    OutputCodeString(#0#0#1#0#47#117#115#114#47#108#105#98#47#108#105#98#83#121#115#116);
    OutputCodeString(#101#109#46#66#46#100#121#108#105#98#0#0#0#0#0#0#38#0#0#0);
    OutputCodeString(#16#0#0#0#56#128#0#0#32#0#0#0#41#0#0#0#16#0#0#0);
    OutputCodeString(#88#128#0#0#0#0#0#0#29#0#0#0#16#0#0#0#208#132#0#0);
    OutputCodeString(#152#1#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#220#0#0#20#228#15#31#248);
    OutputCodeString(#243#15#31#248#251#15#31#248#232#15#31#248#227#15#31#248#226#15#31#248);
    OutputCodeString(#225#15#31#248#224#15#31#248#144#0#128#210#32#0#128#210#225#3#1#145);
    OutputCodeString(#34#0#128#210#1#0#0#212#224#7#65#248#225#7#65#248#226#7#65#248);
    OutputCodeString(#227#7#65#248#232#7#65#248#251#7#65#248#243#7#65#248#228#7#65#248);
    OutputCodeString(#192#3#95#214#243#15#31#248#225#11#64#249#224#19#64#249#31#0#0#241);
    OutputCodeString(#170#0#0#84#224#3#0#203#33#4#0#209#168#5#128#210#226#255#255#151);
    OutputCodeString(#2#0#128#210#224#15#31#248#225#15#31#248#31#0#0#241#160#0#0#84);
    OutputCodeString(#66#4#0#145#72#1#128#210#0#8#200#154#251#255#255#23#95#0#0#241);
    OutputCodeString(#227#23#159#154#66#0#3#139#225#7#65#248#224#7#65#248#33#0#2#203);
    OutputCodeString(#63#0#0#241#237#0#0#84#226#15#31#248#8#4#128#210#206#255#255#151);
    OutputCodeString(#33#4#0#241#161#255#255#84#226#7#65#248#4#0#0#176#132#0#0#145);
    OutputCodeString(#132#0#2#139#132#4#0#209#226#15#31#248#69#1#128#210#3#0#128#210);
    OutputCodeString(#233#3#0#170#0#8#197#154#8#124#5#155#35#1#8#203#97#192#0#145);
    OutputCodeString(#129#0#0#57#132#4#0#209#66#4#0#241#193#254#255#84#226#7#65#248);
    OutputCodeString(#1#0#128#210#4#0#0#176#132#0#0#145#132#0#2#139#1#0#128#82);
    OutputCodeString(#129#0#0#57#144#0#128#210#32#0#128#210#226#3#2#170#1#0#0#176);
    OutputCodeString(#33#0#0#145#1#0#0#212#192#3#95#214#168#1#128#210#171#255#255#151);
    OutputCodeString(#72#1#128#210#169#255#255#151#192#3#95#214#228#15#31#248#243#15#31#248);
    OutputCodeString(#251#15#31#248#232#15#31#248#227#15#31#248#226#15#31#248#225#15#31#248);
    OutputCodeString(#224#15#31#248#0#0#128#210#34#0#128#210#1#0#0#176#33#0#1#145);
    OutputCodeString(#112#0#128#210#16#0#164#242#1#0#0#212#31#0#0#241#225#23#159#26);
    OutputCodeString(#8#0#0#176#8#65#1#145#2#1#64#57#66#0#1#42#2#1#0#57);
    OutputCodeString(#224#7#65#248#225#7#65#248#226#7#65#248#227#7#65#248#232#7#65#248);
    OutputCodeString(#251#7#65#248#243#7#65#248#228#7#65#248#192#3#95#214#8#0#0#176);
    OutputCodeString(#8#33#1#145#8#1#64#57#31#1#0#113#193#0#0#84#220#255#255#151);
    OutputCodeString(#8#0#0#176#8#33#1#145#40#0#128#82#8#1#0#57#192#3#95#214);
    OutputCodeString(#245#255#255#151#8#0#0#176#8#1#1#145#0#1#64#57#210#255#255#151);
    OutputCodeString(#192#3#95#214#239#255#255#151#228#15#31#248#243#15#31#248#251#15#31#248);
    OutputCodeString(#232#15#31#248#227#15#31#248#226#15#31#248#225#15#31#248#224#15#31#248);
    OutputCodeString(#0#0#128#210#34#0#128#210#8#0#0#176#8#65#1#145#8#1#64#57);
    OutputCodeString(#31#1#0#113#65#3#0#84#8#0#0#176#8#1#1#145#8#1#64#57);
    OutputCodeString(#31#129#0#113#104#0#0#84#187#255#255#151#245#255#255#23#8#0#0#176);
    OutputCodeString(#8#1#1#145#8#1#64#57#31#181#0#113#97#0#0#84#226#3#2#203);
    OutputCodeString(#179#255#255#151#8#0#0#176#8#1#1#145#1#1#64#57#63#192#0#113);
    OutputCodeString(#227#0#0#84#63#228#0#113#168#0#0#84#0#124#9#155#0#0#1#139);
    OutputCodeString(#169#255#255#151#246#255#255#23#0#124#2#155#224#7#65#248#225#7#65#248);
    OutputCodeString(#226#7#65#248#227#7#65#248#232#7#65#248#251#7#65#248#243#7#65#248);
    OutputCodeString(#228#7#65#248#192#3#95#214#188#255#255#151#8#0#0#176#8#65#1#145);
    OutputCodeString(#8#1#64#57#31#1#0#113#1#1#0#84#8#0#0#176#8#1#1#145);
    OutputCodeString(#8#1#64#57#31#41#0#113#96#0#0#84#146#255#255#151#244#255#255#23);
    OutputCodeString(#192#3#95#214#0#0#128#210#8#0#0#176#8#65#1#145#0#1#64#57);
    OutputCodeString(#192#3#95#214#169#255#255#151#8#0#0#176#8#1#1#145#0#1#64#57);
    OutputCodeString(#31#40#0#113#224#23#159#154#192#3#95#214#0#0#128#210#48#0#128#210);
    OutputCodeString(#16#0#164#242#1#0#0#212#192#3#95#214#251#3#0#145#19#0#0#176);
    OutputCodeChar(#115);
    OutputCodeChar(#130);
    OutputCodeChar(#1);
    OutputCodeChar(#145);
end;

procedure EmitEndingStub;
begin
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#60#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#224#63#0#0);
    OutputCodeString(#1#0#0#0#136#60#0#0#1#0#0#0#224#60#0#0#1#0#0#0);
    OutputCodeString(#216#61#0#0#1#0#0#0#148#62#0#0#1#0#0#0#172#62#0#0);
    OutputCodeString(#1#0#0#0#120#63#0#0#1#0#0#0#176#63#0#0#1#0#0#0);
    OutputCodeString(#196#63#0#0#1#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#17#34#96#89#0#0#0#0#0#1#95#0#18#0#0#0);
    OutputCodeString(#0#2#0#0#0#3#0#132#121#0#0#2#95#109#104#95#101#120#101#99);
    OutputCodeString(#117#116#101#95#104#101#97#100#101#114#0#9#109#97#105#110#0#13#0#0);
    OutputCodeString(#132#121#4#88#36#12#24#36#20#20#100#20#124#40#4#24#44#48#28#44);
    OutputCodeString(#40#52#4#20#28#20#0#0#0#0#0#0#45#0#0#0#14#1#0#2);
    OutputCodeString(#136#60#0#0#1#0#0#0#58#0#0#0#14#1#0#2#224#60#0#0);
    OutputCodeString(#1#0#0#0#74#0#0#0#14#1#0#2#4#61#0#0#1#0#0#0);
    OutputCodeString(#99#0#0#0#14#1#0#2#16#61#0#0#1#0#0#0#127#0#0#0);
    OutputCodeString(#14#1#0#2#40#61#0#0#1#0#0#0#159#0#0#0#14#1#0#2);
    OutputCodeString(#76#61#0#0#1#0#0#0#186#0#0#0#14#1#0#2#96#61#0#0);
    OutputCodeString(#1#0#0#0#212#0#0#0#14#1#0#2#116#61#0#0#1#0#0#0);
    OutputCodeString(#232#0#0#0#14#1#0#2#216#61#0#0#1#0#0#0#243#0#0#0);
    OutputCodeString(#14#1#0#2#236#61#0#0#1#0#0#0#254#0#0#0#14#1#0#2);
    OutputCodeString(#104#62#0#0#1#0#0#0#11#1#0#0#14#1#0#2#144#62#0#0);
    OutputCodeString(#1#0#0#0#24#1#0#0#14#1#0#2#148#62#0#0#1#0#0#0);
    OutputCodeString(#36#1#0#0#14#1#0#2#172#62#0#0#1#0#0#0#51#1#0#0);
    OutputCodeString(#14#1#0#2#216#62#0#0#1#0#0#0#77#1#0#0#14#1#0#2);
    OutputCodeString(#8#63#0#0#1#0#0#0#107#1#0#0#14#1#0#2#36#63#0#0);
    OutputCodeString(#1#0#0#0#128#1#0#0#14#1#0#2#36#63#0#0#1#0#0#0);
    OutputCodeString(#144#1#0#0#14#1#0#2#80#63#0#0#1#0#0#0#160#1#0#0);
    OutputCodeString(#14#1#0#2#120#63#0#0#1#0#0#0#170#1#0#0#14#1#0#2);
    OutputCodeString(#172#63#0#0#1#0#0#0#181#1#0#0#14#1#0#2#176#63#0#0);
    OutputCodeString(#1#0#0#0#188#1#0#0#14#1#0#2#196#63#0#0#1#0#0#0);
    OutputCodeString(#196#1#0#0#14#1#0#2#224#63#0#0#1#0#0#0#204#1#0#0);
    OutputCodeString(#14#1#0#2#244#63#0#0#1#0#0#0#219#1#0#0#14#1#0#2);
    OutputCodeString(#0#64#0#0#1#0#0#0#237#1#0#0#14#2#0#0#0#64#0#0);
    OutputCodeString(#1#0#0#0#3#2#0#0#14#2#0#2#64#64#0#0#1#0#0#0);
    OutputCodeString(#18#2#0#0#14#2#0#2#72#64#0#0#1#0#0#0#33#2#0#0);
    OutputCodeString(#14#2#0#2#80#64#0#0#1#0#0#0#39#2#0#0#14#2#0#2);
    OutputCodeString(#96#64#0#0#1#0#0#0#56#2#0#0#14#2#0#2#168#64#0#0);
    OutputCodeString(#1#0#0#0#2#0#0#0#15#1#16#0#0#0#0#0#1#0#0#0);
    OutputCodeString(#22#0#0#0#15#1#0#0#132#60#0#0#1#0#0#0#28#0#0#0);
    OutputCodeString(#1#0#0#1#0#0#0#0#0#0#0#0#32#0#95#95#109#104#95#101);
    OutputCodeString(#120#101#99#117#116#101#95#104#101#97#100#101#114#0#95#109#97#105#110#0);
    OutputCodeString(#100#121#108#100#95#115#116#117#98#95#98#105#110#100#101#114#0#82#84#76);
    OutputCodeString(#87#114#105#116#101#67#104#97#114#0#82#84#76#87#114#105#116#101#73#110);
    OutputCodeString(#116#101#103#101#114#0#82#84#76#87#114#105#116#101#73#110#116#101#103#101);
    OutputCodeString(#114#78#111#116#83#105#103#110#101#100#0#82#84#76#87#114#105#116#101#73);
    OutputCodeString(#110#116#101#103#101#114#80#114#101#67#104#101#99#107#76#111#111#112#0#82);
    OutputCodeString(#84#76#87#114#105#116#101#73#110#116#101#103#101#114#80#114#101#67#104#101);
    OutputCodeString(#99#107#76#111#111#112#68#111#110#101#0#82#84#76#87#114#105#116#101#73);
    OutputCodeString(#110#116#101#103#101#114#80#97#100#100#105#110#103#76#111#111#112#0#82#84);
    OutputCodeString(#76#87#114#105#116#101#73#110#116#101#103#101#114#78#111#116#80#97#100#100);
    OutputCodeString(#105#110#103#0#82#84#76#87#114#105#116#101#73#110#116#101#103#101#114#76);
    OutputCodeString(#111#111#112#0#82#84#76#87#114#105#116#101#76#110#0#82#101#97#100#67);
    OutputCodeString(#104#97#114#69#120#0#82#101#97#100#67#104#97#114#73#110#105#116#0#82);
    OutputCodeString(#101#97#100#73#110#105#116#68#111#110#101#0#82#84#76#82#101#97#100#67);
    OutputCodeString(#104#97#114#0#82#84#76#82#101#97#100#73#110#116#101#103#101#114#0#82);
    OutputCodeString(#101#97#100#73#110#116#101#103#101#114#83#107#105#112#87#104#105#116#101#83);
    OutputCodeString(#112#97#99#101#0#82#101#97#100#73#110#116#101#103#101#114#83#107#105#112);
    OutputCodeString(#87#104#105#116#101#83#112#97#99#101#68#111#110#101#0#82#101#97#100#73);
    OutputCodeString(#110#116#101#103#101#114#78#111#116#83#105#103#110#101#100#0#82#101#97#100);
    OutputCodeString(#73#110#116#101#103#101#114#76#111#111#112#0#82#101#97#100#73#110#116#101);
    OutputCodeString(#103#101#114#68#111#110#101#0#82#84#76#82#101#97#100#76#110#0#82#101);
    OutputCodeString(#97#100#76#110#68#111#110#101#0#82#84#76#69#79#70#0#82#84#76#69);
    OutputCodeString(#79#76#78#0#82#84#76#72#97#108#116#0#83#116#117#98#69#110#116#114);
    OutputCodeString(#121#80#111#105#110#116#0#80#114#111#103#114#97#109#69#110#116#114#121#80);
    OutputCodeString(#111#105#110#116#0#82#84#76#87#114#105#116#101#73#110#116#101#103#101#114);
    OutputCodeString(#66#117#102#102#101#114#0#82#101#97#100#67#104#97#114#66#117#102#102#101);
    OutputCodeString(#114#0#82#101#97#100#67#104#97#114#73#110#105#116#101#100#0#73#115#69);
    OutputCodeString(#79#70#0#82#84#76#70#117#110#99#116#105#111#110#84#97#98#108#101#0);
    OutputCodeString(#67#111#112#121#114#105#103#104#116#0#0#0#0#0#0#0#250#222#12#192);
    OutputCodeString(#0#0#1#151#0#0#0#1#0#0#0#0#0#0#0#20#250#222#12#2);
    OutputCodeString(#0#0#1#131#0#2#4#0#0#2#0#2#0#0#0#99#0#0#0#88);
    OutputCodeString(#0#0#0#0#0#0#0#9#0#0#132#208#32#2#0#12#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
    OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#3#124#0#0#0#0);
    OutputCodeString(#0#0#0#1#114#116#108#54#52#109#97#99#79#83#0#228#120#92#77#128);
    OutputCodeString(#120#174#33#101#33#235#127#77#64#33#24#116#146#72#48#1#68#253#172#231);
    OutputCodeString(#196#224#121#152#255#21#171#173#127#172#178#88#111#198#233#102#192#4#215#209);
    OutputCodeString(#209#107#2#79#88#5#255#124#180#124#122#133#218#189#139#72#137#44#167#173);
    OutputCodeString(#127#172#178#88#111#198#233#102#192#4#215#209#209#107#2#79#88#5#255#124);
    OutputCodeString(#180#124#122#133#218#189#139#72#137#44#167#190#35#198#255#224#138#85#135#243);
    OutputCodeString(#117#224#160#213#246#198#47#110#127#23#16#102#34#100#23#66#176#113#41#71);
    OutputCodeString(#100#98#55#127#42#101#48#37#107#218#15#103#204#4#230#196#83#235#184#83);
    OutputCodeString(#164#132#218#21#168#30#215#70#10#155#77#55#113#173#83#173#127#172#178#88);
    OutputCodeString(#111#198#233#102#192#4#215#209#209#107#2#79#88#5#255#124#180#124#122#133);
    OutputCodeString(#218#189#139#72#137#44#167#173#127#172#178#88#111#198#233#102#192#4#215#209);
    OutputCodeString(#209#107#2#79#88#5#255#124#180#124#122#133#218#189#139#72#137#44#167#173);
    OutputCodeString(#127#172#178#88#111#198#233#102#192#4#215#209#209#107#2#79#88#5#255#124);
    OutputCodeString(#180#124#122#133#218#189#139#72#137#44#167#82#147#32#253#65#208#232#82#152);
    OutputCodeString(#165#131#196#113#174#252#210#222#249#44#58#18#96#203#185#164#69#23#253#91);
    OutputCodeChar(#98);
    OutputCodeChar(#100);
    OutputCodeChar(#69);
    OutputCodeChar(#0);
end;

const StartStubSize=16384;
const EndStubSize=18024;


  {__TEXT}
  OffsSegTextVMSize=$88;
  OffsSegTextFileSize=$98;
  ValSegTextVMSize=$4000;
  ValSegTextFileSize=$4000;
  {__text}
  OffsSectTextSize=$D8;
  ValSectTextSize=$37C;
  {__DATA}
  OffsSegDataVMAddr=$118;
  OffsSegDataOffs=$128; 
  ValSegDataVMAddr=$4000; // 1_00004000
  ValSegDataOffs=$4000;
  {__data}
  OffsSectDataVMAddr=$168;
  OffsSectDataOffs=$178;
  ValSectDataVMAddr=$4000;
  ValSectDataOffs=$4000;
  {__LINKEDIT}
  OffsSegLinkVMAddr=$1b0;
  OffsSegLinkOffs=$1c0;
  ValSegLinkVMAddr=$8000;
  ValSegLinkOffs=$8000;
  {LC_DYLD_INFO_ONLY}
  OffsComRebaseOff = $1E8;
  OffsComExportOff = $208;
  ValComRebaseOff = $8000;
  ValComExportOff = $8008;
  {SYMTAB_OFFSETS}
  OffsSymTabOffs=$218;
  OffsStrTabOffs=$220;
  ValSymTabOffs=$8058; 
  ValStrTabOffs=$8288;
  {LC_FUNCTION_STARTS}
  OffsComDataOff = $338;
  ValComDataOff = $8038;
  {LC_CODE_SIGNATURE}
  OffsCodeSignOff = $358;
  ValCodeSignOff = $84D0;
  {SYMTAB_DATA}
  OffsData0=$8200; // rtlWriteIntegerBuffer
  OffsData1=$8210; // ReadCharBuffer
  OffsData2=$8220; // ReadCharInited
  OffsData3=$8230; // IsEOF
  OffsData4=$8240; //RTLFunctionTable
  OffsData5=$8250; //CopyRight
  ValData0=$4000;
  ValData1=$4040;
  ValData2=$4048;
  ValData3=$4050;
  ValData4=$4060;
  ValData5=$40A8;
  {GOT}
  OffsStrings0=$3FF8; 
  OffsStrings1=$3D60;
  OffsStrings2=$3D9C;
  OffsStrings3=$3E14;
  OffsStrings4=$3E30;
  OffsStrings5=$3E68;
  OffsStrings17=$3E80;
  OffsStrings6=$3E98;
  OffsStrings7=$3ED8;
  OffsStrings8=$3EEC;
  OffsStrings9=$E16;//??
  OffsStrings10=$3F08;
  OffsStrings11=$3F24;
  OffsStrings12=$3F7C;
  OffsStrings13=$3F90;
  OffsStrings14=$3FB4;
  OffsStrings15=$3FC8;
  OffsStrings16=$F45; //for test

  ValString0=$130000B0;
  ValString1=$040000B0;
  ValString2=$C1FEFF54;
  ValString3=$010000B0;
  ValString4=$080000B0;
  ValString5=$080000B0;
  ValString17=$080000B0;
  ValString6=$080000B0;
  ValString7=$080000B0;
  ValString8=$080000B0;
  ValString9=$257;
  ValString10=$080000B0;
  ValString11=$080000B0;
  ValString12=$080000B0;
  ValString13=$080000B0;
  ValString14=$080000B0;
  ValString15=$080000B0;
  ValString16=$128;


const locNone=0;
      locPushX0=1;
      locPopX0=2;
      locPopX1=3;
      locIMulEBX=4;
      locXorEDXEDX=5;
      locIDivEBX=6;
      locPushEDX=7;
      locCmpX1X0=8;
      locMovzxEAXAL=9;
      locMovDWordPtrESPEAX=10;
      locJNZJNE0x03=11;
      locMovDWordPtrEBXEAX=12;
      locJmpDWordPtrESIOfs=13;
      locCallDWordPtrESIOfs=14;
      locXChgEDXESI=15;
      locPopESI=16;
      locMovECXImm=17;
      locCLD=18;
      locREPMOVSB=19;
      locTestEAXEAX=20;
      locNegDWordPtrESP=21;
      locMovX0DWordPtrESP=22;
      locMovEBXDWordPtrFORStateCurrentValue=23;
      locCmpDWordPtrEBXEAX=24;
      locMovEAXDWordPtrFORStateDestValue=25;
      {new}
      locJNZJNE0x06=26;

var LastOutputCodeValue,PC:integer;

procedure OCPushX0;
begin
  // WriteLn('str x0, [sp, #-16]!');
  EmitByte($e0);   
  EmitByte($0f);
  EmitByte($1f);
  EmitByte($f8); 
  LastOutputCodeValue:=locPushX0;
end;

procedure OCPushX1;
begin
  // WriteLn('str x1, [sp, #-16]!');
  EmitByte($e1);   
  EmitByte($0f);
  EmitByte($1f);
  EmitByte($f8); 
  LastOutputCodeValue:=locPushX0;
end;

procedure OCPopX0;
begin
  // WriteLn('ldr x0, [sp], #16');
  EmitByte($e0);   
  EmitByte($07);
  EmitByte($41);
  EmitByte($f8); 
  LastOutputCodeValue:=locPopX0;
end;

procedure OCPopX1;
begin
  // WriteLn('ldr x1, [sp], #16');
 EmitByte($e1);   
 EmitByte($07);
 EmitByte($41);
 EmitByte($f8); 
 LastOutputCodeValue:=locPopX1;
end;

procedure OCPopX19;
begin
 // WriteLn('ldr x19, [sp], #16');
 EmitByte($f3);   
 EmitByte($07);
 EmitByte($41);
 EmitByte($f8); 
 LastOutputCodeValue:=locPopESI;
end;

procedure OCXorX0X0;
begin
 // WriteLn('eor x0, x0, x0');
 EmitByte($00);   
 EmitByte($00);
 EmitByte($00);
 EmitByte($ca); 
 LastOutputCodeValue:=locXorEDXEDX;
end;

procedure OCValToReg;
begin
 // WriteLn('mov x12, #1'); 
 EmitByte($2c);   
 EmitByte($00);
 EmitByte($80);
 EmitByte($d2);
end;

procedure OCIDIVX1;
begin
 // WriteLn('sdiv x0, x1, x0');
 EmitByte($20);   
 EmitByte($0c);
 EmitByte($c0);
 EmitByte($9a); 
 LastOutputCodeValue:=locIDivEBX;
end;

procedure OCCmpX1X0;
begin
 // WriteLn('cmp x1, x0');
 EmitByte($3f);   
 EmitByte($00);
 EmitByte($00);
 EmitByte($eb); 
 LastOutputCodeValue:=locCmpX1X0;
end;

// 4000

//  0 AA
//  1 40
//  2 00
//  3 DD
//  4 ВВ
//  5 ВВ
//  6 ВВ
//  7 ВВ
//  8
//  9 
//  10
procedure OCMovDWordPtrX1X0;
begin
 // WriteLn('str x0, [x1]');
 EmitByte($20);   
 EmitByte($00);
 EmitByte($00);
 EmitByte($f9); 
 LastOutputCodeValue:=locMovDWordPtrEBXEAX;
end;

procedure OCNegDWordPtrESP;
begin
  OCPopX0;
  // WriteLn('mvn x0, x0');
  EmitByte($e0);   
  EmitByte($03);
  EmitByte($20);
  EmitByte($aa); 
  OCPushX0;
 LastOutputCodeValue:=locNegDWordPtrESP;
end;

procedure OCMovX0DWordPtrESP;
begin
 // WriteLn('ldr x0, [sp]'); (* MOV X0,DWORD PTR {ESP} *)
 EmitByte($e0);   
 EmitByte($03);
 EmitByte($40);
 EmitByte($f9); 
 LastOutputCodeValue:=locMovX0DWordPtrESP;
end;



procedure OCXChgX4X19;
begin
 // writeLn('mov x8, x4');
 EmitByte($e8);   
 EmitByte($03);
 EmitByte($04);
 EmitByte($aa); 
 // writeLn('mov x4, x19');
 EmitByte($e4);   
 EmitByte($03);
 EmitByte($13);
 EmitByte($aa); 
 // writeLn('mov x19, x8');
 EmitByte($f3);   
 EmitByte($03);
 EmitByte($08);
 EmitByte($aa); 
 LastOutputCodeValue:=locXChgEDXESI;
end;



procedure OCREPMOVSB;
begin
  // Предполагаем, что:
  // x1 = адрес назначения (rbx)
  // x2 = адрес источника (rcx)
  // x3 = количество байт для копирования (rdx)
  WriteLn('cmp x3, #0');     // Проверка, если количество байт больше 0
  WriteLn('beq end_ocrepmovsb'); // Если 0, выходим
  WriteLn('rep_movsb_loop:');
  WriteLn('ldrb w4, [x2], #1'); // Загружаем байт из источника (rcx) в w4 и увеличиваем x2 на 1
  WriteLn('strb w4, [x1], #1 ');   // Сохраняем байт в назначение (rbx) и увеличиваем x1 на 1
  WriteLn('subs x3, x3, #1 ');// Уменьшаем счетчик
  WriteLn('bne rep_movsb_loop'); // Если счетчик не равен 0, продолжаем цикл
  WriteLn('end_ocrepmovsb:');
  WriteLn('ret'); 
//     MOV     X2, X3              // X2 = RCX (количество байтов для копирования)
// 1:  LDRB    W0, [X19], #1      // Загрузить байт из [X19] в W0 и инкрементировать X19
//     STRB    W0, [X4], #1       // Сохранить байт из W0 в [X4] и инкрементировать X4
//     SUBS    X2, X2, #1         // Вычесть 1 из X2
//     BNE     1b                 // Если X2 не ноль, повторить

 LastOutputCodeValue:=locREPMOVSB;
end;

function BitwiseOr(a, b: Integer): Integer;
var
  result, bitPosition: Integer;
begin
  result := 0;
  bitPosition := 1;
  while (a > 0) or (b > 0) do
  begin
    if (a mod 2 > 0) or (b mod 2 > 0) then
      result := result + bitPosition;
    a := a div 2;
    b := b div 2;
    bitPosition := bitPosition * 2;
  end;
  BitwiseOr := result;
end;

function LeftShift(a: Integer; shiftCount: Integer): Integer;
var
  i: Integer;
  result: Integer;
begin
  result := a;
  
  // Просто умножаем число на 2 shiftCount раз
  for i := 1 to shiftCount do
  begin
    result := result * 2;
  end;
  LeftShift := result;
end;

function GetLower16Bits(Value: Integer): Integer;
var
  result: Integer;
begin
  // Извлечение младших 16 бит
  result := Value mod 65536;
  GetLower16Bits := result;  
end;

function GetUpper16Bits(Value: Integer): Integer;
var
  result: Integer;
begin
  // Извлечение старших 16 бит
  result := Value div 65536;
  GetUpper16Bits := result;
end;


procedure OCMovValueX6(Value:integer);
var val, result, mask, i : integer;
begin
 // MOVZ x6, #Value, lsl #16
 // MOVK x6, #Value, lsl #0
  // EmitInt32(BitwiseOr($D2A00006, LeftShift(GetUpper16Bits(Value), 5)));
  // EmitInt32(BitwiseOr($F2A00006, LeftShift(GetLower16Bits(Value), 5)));
  mask := $D2A00006; 
  val := LeftShift(GetUpper16Bits(Value), 5);
  EmitByte(BitwiseOr(mask mod 256, val mod 256));
  mask := mask div 256;
  val := val div 256;
  for i := 1 to 3 do
  begin
    EmitByte(BitwiseOr(mask  mod 256, val mod 256));
   // writeln(BitwiseOr(mask mod 256, val mod 256), ' bitOr');
    mask := mask div 256;
    val := val div 256;
  end;

  mask := $F2A00006; 
  val := LeftShift(GetLower16Bits(Value), 5);
  EmitByte(BitwiseOr(mask mod 256, val mod 256));
  mask := mask div 256;
  val := val div 256;
  for i := 1 to 3 do
  begin
    EmitByte(BitwiseOr(mask  mod 256, val mod 256));
    //writeln(BitwiseOr(mask mod 256, val mod 256), ' bitOr');
    mask := mask div 256;
    val := val div 256;
  end;
  //EmitInt32(BitwiseOr($F2A00000, LeftShift(GetLower16Bits(Value), 5)));
end;

procedure OCMovValueX0(Value:integer);
var val, result, mask, i : integer;
begin
 // MOVZ x0, #Value, lsl #16
 // MOVK x0, #Value, lsl #0
  mask := $D2A00000; 
  val := LeftShift(GetUpper16Bits(Value), 5);
  EmitByte(BitwiseOr(mask mod 256, val mod 256));
  mask := mask div 256;
  val := val div 256;
  for i := 1 to 3 do
  begin
    EmitByte(BitwiseOr(mask  mod 256, val mod 256));
   // writeln(BitwiseOr(mask mod 256, val mod 256), ' bitOr');
    mask := mask div 256;
    val := val div 256;
  end;

  mask := $F2A00000; 
  val := LeftShift(GetLower16Bits(Value), 5);
  EmitByte(BitwiseOr(mask mod 256, val mod 256));
  mask := mask div 256;
  val := val div 256;
  for i := 1 to 3 do
  begin
    EmitByte(BitwiseOr(mask  mod 256, val mod 256));
    //writeln(BitwiseOr(mask mod 256, val mod 256), ' bitOr');
    mask := mask div 256;
    val := val div 256;
  end;
  //EmitInt32(BitwiseOr($F2A00000, LeftShift(GetLower16Bits(Value), 5)));

end;

//TODO
procedure OCMovX2Imm(Value:integer);
begin
//MOV RCX, Value
 OCMovValueX6(Value);
 //WriteLn('mov x2, x6');
 LastOutputCodeValue:=locMovECXImm;
end;


procedure OCCallDWordPtrX19Ofs(Ofs:integer);
begin

 // ldr x5, [x19]
 // br x5
 // Todo: x19 + sizeof(addr) * Ofs
 EmitByte($60);
 EmitByte($02);
 EmitByte($1F);
 EmitByte($D6);
 LastOutputCodeValue:=locCallDWordPtrESIOfs;
end;

var JumpTable:array[1:MaximalCodeSize] of integer;

procedure AssembleAndLink;
var
   InjectionSize,
   CountJumps,Opcode,Value,Index,PEEXECodeSize,PEEXESectionVirtualSize,
   PEEXESectionAlignment,PEEXECodeStart, iter :integer;
begin
 EmitStubCode;
 PEEXECodeStart:=OutputCodeDataSize;
 LastOutputCodeValue:=locNone;
 PC:=0;
 CountJumps:=0;
 // WriteLn('mov x27, sp');
//  EmitByte($fb);   
//  EmitByte($03);
//  EmitByte($00);
//  EmitByte($91);
 while PC<CodePosition do begin
  Opcode:=Code[PC];
  // WriteLn('l_', Opcode, ':');
  Value:=Code[PC+1];
  Code[PC]:=OutputCodeDataSize;

  case Opcode of
   OPAdd:begin
    OCPopX0;
    OCPopX1;
    // WriteLn('add x0, x0, x1');
    EmitByte($00);   
    EmitByte($00);
    EmitByte($01);
    EmitByte($8b); 
    // WriteLn('str x0, [sp, #-16]!');
    EmitByte($e0);   
    EmitByte($0f);
    EmitByte($1f);
    EmitByte($f8); 
    LastOutputCodeValue:=locNone;
   end;
   OPNeg:begin
    OCNegDWordPtrESP;
   end;
   OPMul:begin
    OCPopX1;
    OCPopX0;
    // WriteLn('mul x0, x0, x1');
    EmitByte($00);   
    EmitByte($7c);
    EmitByte($01);
    EmitByte($9b); 
    OCPushX0;
   end;
   OPDivD:begin
    OCPopX1;
    OCPopX0;
    OCIDIVX1;
    OCPushX0;
   end;
   OPRemD:begin
    OCPopX1;
    OCPopX0;
    OCXorX0X0;
    OCIDIVX1;
    OCPushX0;
   end;
   OPDiv2:begin
    // WriteLn('asr x0, x0, #2');
    EmitByte($00);   
    EmitByte($fc);
    EmitByte($42);
    EmitByte($93); 
    LastOutputCodeValue:=locNone;
   end;
   OPRem2:begin
    OCPopX1;
    OCPopX0;
    OCXorX0X0;
    OCIDIVX1;
    OCPushX0;
   end;
   OPEqlI:begin
    OCPopX1;
    OCPopX0;
    OCCmpX1X0;
    OCValToReg;
    // WriteLn('csel x0, xzr, x12, eq');
    EmitByte($e0);   
    EmitByte($03);
    EmitByte($8c);
    EmitByte($9a);
    LastOutputCodeValue:=locNone;
    OCPushX0;
   end;
   OPNEqI:begin
    OCPopX1;
    OCPopX0;
    OCCmpX1X0;
    OCValToReg;
    // WriteLn('csel x0, xzr, x12, ne');
    EmitByte($e0);   
    EmitByte($13);
    EmitByte($8c);
    EmitByte($9a);
    LastOutputCodeValue:=locNone;
    OCPushX0;
   end;
   OPLssI:begin
    OCPopX1;
    OCPopX0;
    OCCmpX1X0;
    OCValToReg;
    // WriteLn('csel x0, xzr, x12, lt');
    EmitByte($e0);   
    EmitByte($b3);
    EmitByte($8c);
    EmitByte($9a);
    LastOutputCodeValue:=locNone;
    OCPushX0;
   end;
   OPLeqI:begin
    OCPopX1;
    OCPopX0;
    OCCmpX1X0;
    OCValToReg;
    // WriteLn('csel x0, xzr, x12, le');
    EmitByte($e0);   
    EmitByte($d3);
    EmitByte($8c);
    EmitByte($9a);
    LastOutputCodeValue:=locNone;
    OCPushX0;
   end;
   OPGtrI:begin
    OCPopX1;
    OCPopX0;
    OCCmpX1X0;
    OCValToReg;
    // WriteLn('csel x0, xzr, x12, gt');
    EmitByte($e0);   
    EmitByte($c3);
    EmitByte($8c);
    EmitByte($9a);
    LastOutputCodeValue:=locNone;
    OCPushX0;
   end;
   OPGEqi:begin
    OCPopX1;
    OCPopX0;
    OCCmpX1X0;
    OCValToReg;
    // WriteLn('csel x0, xzr, x12, ge');
    EmitByte($e0);   
    EmitByte($a3);
    EmitByte($8c);
    EmitByte($9a);
    LastOutputCodeValue:=locNone;
    OCPushX0;
   end;
   OPDupl:begin
    OCPopX0;
    OCPushX0;
    OCPushX0;
    LastOutputCodeValue:=locNone;
   end;
   OPSwap:begin
    OCPopX1;
    OCPopX0;
    OCPushX1;
    LastOutputCodeValue:=locNone;
    OCPushX0;
   end;
   OPAndB:begin
    OCPopX0;
    OCPopX1;
    // WriteLn('cmp x1, 0');
    EmitByte($3f);   
    EmitByte($00);
    EmitByte($00);
    EmitByte($f1);
    // WriteLn('csel x0, xzr, x0, eq');
    EmitByte($e0);   
    EmitByte($03);
    EmitByte($80);
    EmitByte($9a);
    OCPushX0;
    LastOutputCodeValue:=locNone;
   end;
   OPOrB:begin
    OCPopX0;
    OCPopX1;
    // WriteLn('cmp x1, 1');
    EmitByte($3f);   
    EmitByte($04);
    EmitByte($00);
    EmitByte($f1);
    OCValToReg;
    // WriteLn('csel x0, x12, x0, eq');
    EmitByte($80);   
    EmitByte($01);
    EmitByte($80);
    EmitByte($9a);
    OCPushX0;
    LastOutputCodeValue:=locNone;
   end;
   OPLoad:begin
    OCPopX0;
    // WriteLn('ldr x1, [x0]');
    EmitByte($01);   
    EmitByte($00);
    EmitByte($40);
    EmitByte($f9);
    OCPushX1;
   end;
   OPStore:begin
    OCPopX1;
    OCPopX0;
    OCMovDWordPtrX1X0;
   end;
   OPHalt:begin
    OCCallDWordPtrX19Ofs(0);   
   end;
   OPWrI:begin
    OCCallDWordPtrX19Ofs(16);
   end;
   OPWrC:begin
    OCCallDWordPtrX19Ofs(8);
   end;
   OPWrL:begin
    OCCallDWordPtrX19Ofs(24);
   end;
   OPRdI:begin
    OCPopX1;
    OCCallDWordPtrX19Ofs(40);
    OCMovDWordPtrX1X0;
   end;
   OPRdC:begin
    OCPopX1;
    OCCallDWordPtrX19Ofs(32);
    OCMovDWordPtrX1X0;
   end;
   OPRdL:begin
    OCCallDWordPtrX19Ofs(48);
   end;
   OPEOF:begin
    OCCallDWordPtrX19Ofs(56);
    OCPushX0;
   end;
   OPEOL:begin
    OCCallDWordPtrX19Ofs(64);
    OCPushX0;
   end;
   OPLdC:begin
    // WriteLn('mov x0, #', Value);
    OCMovValueX0(Value);
    OCPushX0;
    LastOutputCodeValue:=locNone;
    PC:=PC+1;
   end;
   OPLdA:begin
    Value := Value * 4;
    if Value = 0 then begin
        // WriteLn('mov x0, x27');
        EmitByte($e0);   
        EmitByte($03);
        EmitByte($1b);
        EmitByte($aa);
    end else begin
        OCMovValueX6(Value);
        // WriteLn('add x0, x27, x6');
        EmitByte($60);   
        EmitByte($03);
        EmitByte($06);
        EmitByte($8b);

    end;
    LastOutputCodeValue := locNone;
    OCPushX0;
    PC := PC + 1;
   end;
   OPLdLA:begin
    Value := Value * 4;
    if Value=0 then begin
      // WriteLn('mov x0, sp');
      EmitByte($e0);   
      EmitByte($03);
      EmitByte($00);
      EmitByte($91);
    end else begin
      OCMovValueX6(Value);
      // WriteLn('add x0, sp, x6');
      EmitByte($e0);   
      EmitByte($63);
      EmitByte($26);
      EmitByte($8b);
    end;
    LastOutputCodeValue:=locNone;
    OCPushX0;
    PC:=PC+1;
   end;
   OPLdL:begin
    Value:=Value*4;
    if Value=0 then begin
     OCMovX0DWordPtrESP;
    end else begin
      OCMovValueX6(Value);
      // WriteLn('ldr x0, [sp, x6]');
      EmitByte($e0);   
      EmitByte($6b);
      EmitByte($66);
      EmitByte($f8);
    end;
    OCPushX0;
    PC:=PC+1;
   end;
   OPLdG:begin
    Value:=Value*4;
    OCMovValueX6(Value);
    // WriteLn('ldr x0, [x27, x6]');
    EmitByte($60);   
    EmitByte($6b);
    EmitByte($66);
    EmitByte($f8);
    OCPushX0;
    LastOutputCodeValue:=locNone;
    PC:=PC+1;
   end;
   OPStL:begin
    OCPopX0;
    Value:=Value-8;
    Value:=Value*4;
    if Value=0 then begin
      // WriteLn('str x0, [sp]'); { MOV DWORD PTR [ESP],X0 }
      EmitByte($e0);   
      EmitByte($03);
      EmitByte($00);
      EmitByte($f9);
    end else if (Value>=-128) and (Value<=127) then begin
     OCMovValueX6(Value);
     // WriteLn('str x0, [sp, x6]'); { MOV DWORD PTR [ESP+BYTE Value],X0 }
     EmitByte($e0);   
     EmitByte($6b);
     EmitByte($26);
     EmitByte($f8);
    end else begin
      OCMovValueX6(Value);
      // WriteLn('ldr x0, [sp, x6]'); { MOV X0,DWORD PTR [ESP+DWORD Value] }\
      EmitByte($e0);   
      EmitByte($6b);
      EmitByte($66);
      EmitByte($f8);
    end;
    LastOutputCodeValue:=locNone;
    PC:=PC+1;
   end;
   OPStG:begin
    OCPopX0;
    Value:=Value*4;
    OCMovValueX6(Value);
    //WriteLn('str x0, [x27, x6]');
    EmitByte($60);   
    EmitByte($6b);
    EmitByte($26);
    EmitByte($f8);
    LastOutputCodeValue:=locNone;
    PC:=PC+1;
   end;
   OPMove:begin
    // EDX = rdi = x4 ESI = rsi = x19 (пусть будет так) x14 = edi
    OCXChgX4X19;
    // EmitByte($5f); { POP EDI }
    //writeLn('ldr x14, [sp], #16');
    LastOutputCodeValue:=locNone;
    OCPopX19;
    Value:=Value*4;
    OCMovX2Imm(Value);
    OCREPMOVSB;
    OCXChgX4X19;
    PC:=PC+1;
   end;
   OPCopy:begin
    OCXChgX4X19;
    OCPopX19;
    Value:=Value*4;
    OCMovX2Imm(Value);
    // EmitByte($48); EmitByte($29); EmitByte($cc); { SUB ESP,ECX }
    writeLn('sub sp, sp, x2');
    // EmitByte($48); EmitByte($89); EmitByte($e7); { MOV EDI,ESP }
    writeLn('mov x14, sp');
    LastOutputCodeValue:=locNone;
    OCREPMOVSB;
    OCXChgX4X19;
    PC:=PC+1;
   end;
   OPAddC:begin
    OCPopX0;
    OCMovValueX6(Value);
    // WriteLn('add x0, x0, x6');
    EmitByte($00);   
    EmitByte($00);
    EmitByte($06);
    EmitByte($8b);
    OCPushX0;
    LastOutputCodeValue:=locNone;
    PC:=PC+1;
   end;
   OPMulC:begin
    OCPopX0;
    OCMovValueX6(Value);
    // WriteLn('mov x1, x6');
    EmitByte($e1);   
    EmitByte($03);
    EmitByte($06);
    EmitByte($aa);
    // WriteLn('mul x0, x0, x1');
    EmitByte($00);   
    EmitByte($7c);
    EmitByte($01);
    EmitByte($9b);
    OCPushX0;
    PC:=PC+1;
   end;
   OPJmp:begin
    if Value<>(PC+2) then begin
     CountJumps:=CountJumps+1;

     JumpTable[CountJumps]:=OutputCodeDataSize+1;
     EmitInt32(Value);
    end; 
    PC:=PC+1;
    LastOutputCodeValue:=locNone;
   end;
   OPJZ:begin
    CountJumps:=CountJumps+1;
    OCPopX0;
    // WriteLn('cmp x0, xzr');
    EmitByte($1f);   
    EmitByte($00);
    EmitByte($1f);
    EmitByte($eb);
    //WriteLn('b.eq l_', Value);
    JumpTable[CountJumps]:= PC div 2 + 1;
    LastOutputCodeValue:=locNone;
    PC:=PC+1;
   end;
   OPCall:begin
    CountJumps:=CountJumps+1;
    //WriteLn('bl l_', Value);
    JumpTable[CountJumps]:= PC div 2 + 1;
    LastOutputCodeValue:=locNone;
    PC:=PC+1;
   end;
   OPAdjS:begin
    Value:=Value*(-4);
    OCMovValueX6(Value);
    // WriteLn('sub sp, sp, x6');
    EmitByte($ff);   
    EmitByte($63);
    EmitByte($26);
    EmitByte($cb);
    LastOutputCodeValue:=locNone;
    PC:=PC+1;
   end;
   OPExit:begin
    Value:= Value-4;
    Value:= Value*4;
    if Value>0 then begin
      //WriteLn('add sp, sp, #', Value);
      EmitInt32(BitwiseOr($3FF00019, LeftShift(Value, 10)));
      // WriteLn('ret');
      EmitByte($c0);   
      EmitByte($03);
      EmitByte($5f);
      EmitByte($d6);
    end else if Value=0 then begin
     // WriteLn('ret'); { RET }
     EmitByte($c0);   
     EmitByte($03);
     EmitByte($5f);
     EmitByte($d6);
    end else begin
     Error(145);
    end;
    LastOutputCodeValue:=locNone;
    PC:=PC+1;
   end;
  end;
  PC:=PC+1;
 end;

  { Patch jumps + calls }
  for Index:=1 to CountJumps do begin
 	Value:=JumpTable[Index];
 	{jump relative for x64 is limited to 32bit value}
  	OutputCodePutInt32(Value,(   (   Code[OutputCodeGetInt32(Value)] - Value) - 3));
  end;

{	function to it's offset in x64 and x32 mapping }
{ func 	    x64		x32
	halt 	0		0
	wrChar 	8		4
	wrInt 	10		8
	wrLn 	18		C
	rdChar 	20		10
	rdInt 	28		14
	rdLn 	30		18
	EOF 	38		1C
	EOLN 	40		20
}

  {injecting}
  InjectionSize:=OutputCodeDataSize-StartStubSize; 
  iter := InjectionSize mod 16384;
  while iter <= 16383 do
  begin
    // EmitByte($1F);
    // EmitByte($20);
    // EmitByte($03);
    // EmitByte($D5);
    EmitInt32($D503201F);
    iter := iter + 4;   
  end;
  // writeln('InjectionSize= ', InjectionSize);
  {new}
  EmitEndingStub;

  {injecting}
  InjectionSize:=OutputCodeDataSize-EndStubSize-StartStubSize; // всегда кратен 16384
  // writeln('InjectionSize= ', InjectionSize);
//    InjectionSize= 32
// InjectionSize= 16384
  // OutputCodeDataSize - количество всего вставленных байтов 
  // EndStubSize количество байтов во 2 части rtl
  // StartStubSize - количество байтов в 1 части rtl

  

  {__TEXT}
  OutputCodePutInt32(OffsSegTextVMSize + $1, 		  ValSegTextVMSize + InjectionSize); // ++++++
  OutputCodePutInt32(OffsSegTextFileSize + $1, 		  ValSegTextFileSize + InjectionSize); // ++++

  {__text}
  OutputCodePutInt32(OffsSectTextSize + $1, 		  ValSectTextSize + InjectionSize);   // not checked +15

  {__DATA}
  OutputCodePutInt32(OffsSegDataVMAddr + $1, 		  ValSegDataVMAddr + InjectionSize); // +++++
  OutputCodePutInt32(OffsSegDataOffs + $1, 		  ValSegDataOffs + InjectionSize  ); // +++++


  {__data}
  OutputCodePutInt32(OffsSectDataVMAddr + $1, 		  ValSectDataVMAddr + InjectionSize); // 8000 + 15 -- работает верно
  OutputCodePutInt32(OffsSectDataOffs + $1, 		  ValSectDataOffs + InjectionSize); // ++

  {__LINKEDIT}
 OutputCodePutInt32(OffsSegLinkVMAddr + $1, 		  ValSegLinkVMAddr + InjectionSize);
 OutputCodePutInt32(OffsSegLinkOffs + $1, 		  ValSegLinkOffs + InjectionSize);
 {LC_DYLD_INFO_ONLY}
  OutputCodePutInt32(OffsComRebaseOff + $1, 		  ValComRebaseOff + InjectionSize);
  OutputCodePutInt32(OffsComExportOff + $1, 		  ValComExportOff + InjectionSize);
  {SYMTAB_OFFSETS}
 OutputCodePutInt32(OffsSymTabOffs + $1, 		  ValSymTabOffs + InjectionSize);
 OutputCodePutInt32(OffsStrTabOffs + $1, 		  ValStrTabOffs + InjectionSize);
   {LC_FUNCTION_STARTS}
  OutputCodePutInt32(OffsComDataOff + $1, 		  ValComDataOff + InjectionSize);
    {LC_CODE_SIGNATURE}
  OutputCodePutInt32(OffsCodeSignOff + $1, 		  ValCodeSignOff + InjectionSize);
//  {SYMTAB_DATA}
  OutputCodePutInt32(OffsData1 + InjectionSize + $1, 		  ValData1 + InjectionSize);
  OutputCodePutInt32(OffsData0 + InjectionSize + $1, 		  ValData0 + InjectionSize);
  OutputCodePutInt32(OffsData2 + InjectionSize + $1, 		  ValData2 + InjectionSize);
  OutputCodePutInt32(OffsData3 + InjectionSize + $1, 		  ValData3 + InjectionSize);
  OutputCodePutInt32(OffsData4 + InjectionSize + $1, 		  ValData4 + InjectionSize);
 OutputCodePutInt32(OffsData5 + InjectionSize + $1, 		  ValData5 + InjectionSize);

  // {GOT}
  OutputCodePutInt32(OffsStrings0 + $3 + $1, 		  ValString0 + InjectionSize);
  OutputCodePutInt32(OffsStrings1 + $3 + $1, 		  ValString1 + InjectionSize);
  OutputCodePutInt32(OffsStrings2 + $3 + $1, 		  ValString2 + InjectionSize);
  OutputCodePutInt32(OffsStrings3 + $3 + $1, 		  ValString3 + InjectionSize);
  OutputCodePutInt32(OffsStrings4 + $3 + $1, 		  ValString4 + InjectionSize);
  OutputCodePutInt32(OffsStrings5 + $3 + $1, 		  ValString5 + InjectionSize);
  OutputCodePutInt32(OffsStrings6 + $3 + $1, 		  ValString6 + InjectionSize);
  OutputCodePutInt32(OffsStrings7 + $3 + $1, 		  ValString7 + InjectionSize);
  OutputCodePutInt32(OffsStrings8 + $3 + $1, 		  ValString8 + InjectionSize);
  // OutputCodePutInt32(OffsStrings9 + $3 + $1, 		  ValString9 + InjectionSize);
  OutputCodePutInt32(OffsStrings10 + $3 + $1, 		  ValString10 + InjectionSize);
  OutputCodePutInt32(OffsStrings11 + $3 + $1, 		  ValString11 + InjectionSize);
  OutputCodePutInt32(OffsStrings12 + $3 + $1, 		  ValString12 + InjectionSize);
  OutputCodePutInt32(OffsStrings13 + $3 + $1, 		  ValString13 + InjectionSize);
  OutputCodePutInt32(OffsStrings14 + $3 + $1, 		  ValString14 + InjectionSize);
  OutputCodePutInt32(OffsStrings15 + $3 + $1, 		  ValString15 + InjectionSize);
  // OutputCodePutInt32(OffsStrings16 + $3 + $1, 		  ValString16 + InjectionSize);// test
   OutputCodePutInt32(OffsStrings17 + $3 + $1, 		  ValString17 + InjectionSize);

 WriteOutputCode;
end;

begin
 StringCopy(Keywords[SymBEGIN],'BEGIN               ');
 StringCopy(Keywords[SymEND],'END                 ');
 StringCopy(Keywords[SymIF],'IF                  ');
 StringCopy(Keywords[SymTHEN],'THEN                ');
 StringCopy(Keywords[SymELSE],'ELSE                ');
 StringCopy(Keywords[SymWHILE],'WHILE               ');
 StringCopy(Keywords[SymDO],'DO                  ');
 StringCopy(Keywords[SymCASE],'CASE                ');
 StringCopy(Keywords[SymREPEAT],'REPEAT              ');
 StringCopy(Keywords[SymUNTIL],'UNTIL               ');
 StringCopy(Keywords[SymFOR],'FOR                 ');
 StringCopy(Keywords[SymTO],'TO                  ');
 StringCopy(Keywords[SymDOWNTO],'DOWNTO              ');
 StringCopy(Keywords[SymNOT],'NOT                 ');
 StringCopy(Keywords[SymDIV],'DIV                 ');
 StringCopy(Keywords[SymMOD],'MOD                 ');
 StringCopy(Keywords[SymAND],'AND                 ');
 StringCopy(Keywords[SymOR],'OR                  ');
 StringCopy(Keywords[SymCONST],'CONST               ');
 StringCopy(Keywords[SymVAR],'VAR                 ');
 StringCopy(Keywords[SymTYPE],'TYPE                ');
 StringCopy(Keywords[SymARRAY],'ARRAY               ');
 StringCopy(Keywords[SymOF],'OF                  ');
 StringCopy(Keywords[SymPACKED],'PACKED              ');
 StringCopy(Keywords[SymRECORD],'RECORD              ');
 StringCopy(Keywords[SymPROGRAM],'PROGRAM             ');
 StringCopy(Keywords[SymFORWARD],'FORWARD             ');
 StringCopy(Keywords[SymHALT],'HALT                ');
 StringCopy(Keywords[SymFUNC],'FUNCTION            ');
 StringCopy(Keywords[SymPROC],'PROCEDURE           ');

 Types[TypeINT].Size:=4;
 Types[TypeINT].Kind:=KindSIMPLE;
 Types[TypeCHAR].Size:=4;
 Types[TypeCHAR].Kind:=KindSIMPLE;
 Types[TypeBOOL].Size:=4;
 Types[TypeBOOL].Kind:=KindSIMPLE;
 Types[TypeSTR].Size:=0;
 Types[TypeSTR].Kind:=KindSIMPLE;
 TypePosition:=4;

 SymbolNameList[-1]:=0;
 CurrentLevel:=-1;
 IdentifierPosition:=0;

 EnterSymbol('FALSE               ',IdCONST,TypeBOOL);
 Identifiers[IdentifierPosition].Value:=ord(false);

 EnterSymbol('TRUE                ',IdCONST,TypeBOOL);
 Identifiers[IdentifierPosition].Value:=ord(true);

 EnterSymbol('MAXINT              ',IdCONST,TypeINT);
 Identifiers[IdentifierPosition].Value:=2147483647;

 EnterSymbol('INTEGER             ',IdTYPE,TypeINT);
 EnterSymbol('CHAR                ',IdTYPE,TypeCHAR);
 EnterSymbol('BOOLEAN             ',IdTYPE,TypeBOOL);

 EnterSymbol('CHR                 ',IdFUNC,TypeCHAR);
 Identifiers[IdentifierPosition].FunctionLevel:=-1;
 Identifiers[IdentifierPosition].FunctionAddress:=FunCHR;
 Identifiers[IdentifierPosition].Inside:=false;

 EnterSymbol('ORD                 ',IdFUNC,TypeINT);
 Identifiers[IdentifierPosition].FunctionLevel:=-1;
 Identifiers[IdentifierPosition].FunctionAddress:=FunORD;
 Identifiers[IdentifierPosition].Inside:=false;

 EnterSymbol('WRITE               ',IdFUNC,0);
 Identifiers[IdentifierPosition].FunctionLevel:=-1;
 Identifiers[IdentifierPosition].FunctionAddress:=FunWRITE;

 EnterSymbol('WRITELN             ',IdFUNC,0);
 Identifiers[IdentifierPosition].FunctionLevel:=-1;
 Identifiers[IdentifierPosition].FunctionAddress:=FunWRITELN;

 EnterSymbol('READ                ',IdFUNC,0);
 Identifiers[IdentifierPosition].FunctionLevel:=-1;
 Identifiers[IdentifierPosition].FunctionAddress:=FunREAD;

 EnterSymbol('READLN              ',IdFUNC,0);
 Identifiers[IdentifierPosition].FunctionLevel:=-1;
 Identifiers[IdentifierPosition].FunctionAddress:=FunREADLN;

 EnterSymbol('EOF                 ',IdFUNC,TypeBOOL);
 Identifiers[IdentifierPosition].FunctionLevel:=-1;
 Identifiers[IdentifierPosition].FunctionAddress:=FunEOF;
 Identifiers[IdentifierPosition].Inside:=false;

 EnterSymbol('EOLN                ',IdFUNC,TypeBOOL);
 Identifiers[IdentifierPosition].FunctionLevel:=-1;
 Identifiers[IdentifierPosition].FunctionAddress:=FunEOFLN;
 Identifiers[IdentifierPosition].Inside:=false;

 SymbolNameList[0]:=0;
 CurrentLevel:=0;

 CurrentLine:=1;
 CurrentColumn:=0;

 ReadChar;
 GetSymbol;
 IsLabeled:=true;
 CodePosition:=0;
 LastOpcode:=-1;
 StackPosition:=4;
 Expect(SymPROGRAM);
 Expect(TokIdent);
 Expect(TokSemi);
 EmitOpcode(OPJmp,0);
 Block(0);
 EmitOpcode2(OPHalt);
 Check(TokPeriod);
 AssembleAndLink;
end.


// clang -o ./target/rtl64macOS ./target/rtl64macOS.o -Wl,-no_deduplicate -Wl,-no_compact_unwind -Wl,-no_fixup_chains \
//    -lSystem -isysroot `xcrun -sdk macosx --show-sdk-path`