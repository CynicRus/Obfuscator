unit ob_obfuscator;
{**
* This file is part of the Obfuscator for PascalScript.
* Simba Obfuscator. is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* Simba Obfuscator. is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Simba Obfuscator. If not, see <http://www.gnu.org/licenses/>.
*}
{$mode objfpc}{$H+}

interface

uses
  ob_analyzer,Math, SysUtils;

const
{$WARNINGS OFF}
  Chars: set of Char = ['(', ')', '+', '-', '*', '/', ';', '=', '[', ']',
    ',', '@', ':', '.', '<', '>', '#', '$', '''', '"', '{'];
{$WARNINGS ON}
  Reserved: array [0 .. 30] of string = ('program', 'var', 'const', 'begin',
    'end', 'if', 'then', 'while', 'do', 'repeat', 'until', 'case', 'of', 'for',
    'to', 'as', 'is', 'mod', 'div', 'or', 'and', 'xor', 'uses', 'type',
    'function', 'procedure', 'string', 'nil','pointer','out','forward');
  type
  ObfuscationOptions =
    (
    ooUnreadable,
    // ooInvisible, // невидимые имена , будут, если будет юникод
    ooMuchBrackets
    );

const
  SetOptions:
    set of ObfuscationOptions = [ooUnreadable, ooMuchBrackets];

  type

  TBeginEnd = record
    beginPos, endPos: Integer;
  end;

  TBEArray = array of TBeginEnd;
  TStrArray = array of String;
  TIdArray = record
    I: TStrArray;
  end;



  TMArray = record
    Names: TStrArray;
    Positions: TBEArray;
  end;

  TObfuscator = class(TObject)
  private
    FLexems: TLexems;
    FLC: Integer;
    FOutText: string;
    FInnerText: string;
    GlobalConsts: TIdArray;
    GlobalVars: TIdArray;
    Methods: TMArray;
    FAnalyzer: TLexicalAnalyzer;
    procedure FindConsts(var constsArray: TStrArray; const pos: Integer);

    procedure FindVars(var varsArray: TStrArray; const apos: Integer);

    procedure ObfuscateNumbers; //

    procedure ObfuscateMethods;

    procedure ObfuscateGlobals;

    procedure GetGlobal; //
    procedure GetMethods; //
    function GenerateUnRdblName: string; //
{
    function GenerateInvslbName: string; //
}
    function AddBrackets(const Src: string): string; //
    function FindLexem(const Name: string; const Offset: Cardinal = 0): Cardinal; //
    function IsReserved(const Name: string): boolean;
    function GenerateExpr(const num: string): string;
    procedure SetInText(txt: string);
  public
    procedure Obfuscate;
    constructor Create;
    destructor Destroy; override;
    property OutText: string read FOutText;
    property InnerText: string read FInnerText write FInnerText;
  end;

function MArray(Name: string; Position: TBeginEnd): TMArray;

Operator := (A : TStrArray) R : TIDArray;
Operator + (left,right: TIdArray): TIdArray;
Operator + (left,right: TMArray): TMArray;

implementation

function MArray(Name: string; Position: TBeginEnd): TMArray;
var
  tmp: TMArray;
begin
  SetLength(tmp.Names, 1);
  SetLength(tmp.Positions, 1);
  tmp.Names[0] := Name;
  tmp.Positions[0] := Position;
  Result := tmp;
end;

operator:=(A: TStrArray)R: TIDArray;
var
  j: Integer;
  t: TIdArray;
begin
  SetLength(t.I, Length(a));
  for j := 0 to Length(a) - 1 do
    t.I[j] := a[j];
  R := t;
end;

operator+(left, right: TIdArray): TIdArray;
var
  j: Integer;
  t: TIdArray;
begin
  SetLength(t.I, Length(left.I) + Length(right.I));
  for j := 0 to Length(left.I) - 1 do
    t.I[j] := left.I[j];
  for j := Length(left.I) to Length(t.I) - 1 do
    t.I[j] := right.I[j - Length(left.I)];
  Result := t;
end;

operator+(left, right: TMArray): TMArray;
var
  j: Integer;
  t: TMArray;
begin
  SetLength(t.Names, Length(left.Names) + Length(right.Names));
  SetLength(t.Positions, Length(left.Positions) + Length(right.Positions));
  for j := 0 to Length(left.Names) - 1 do
  begin
    t.Names[j] := left.Names[j];
    t.Positions[j] := left.Positions[j];
  end;
  for j := Length(left.Names) to Length(t.Names) - 1 do
  begin
    t.Names[j] := right.Names[j - Length(left.Names)];
    t.Positions[j] := right.Positions[j - Length(left.Positions)];
  end;
  Result := t;
end;

function TObfuscator.AddBrackets(const Src: string): string;
var
  brCount, I: Integer;
  tmpStr: string;
begin
  tmpStr := Src;
  Randomize;
  brCount := Random(21) + Random(21);
  for I := 1 to brCount do
    tmpStr := '(' + tmpStr;
  for I := 1 to brCount do
    tmpStr := tmpStr + ')';
  Result := tmpStr;
end;

constructor TObfuscator.Create;
begin
  inherited;
  FAnalyzer := TLexicalAnalyzer.Create;
  FLexems := nil;
  FLC := 0;
  FOutText := '';
  FInnerText := '';
end;

destructor TObfuscator.Destroy;
begin
  FAnalyzer.Destroy;
  FLexems := nil;
  FLC := 0;
  FOutText := '';
  FInnerText := '';
  inherited;
end;

procedure TObfuscator.FindConsts(var constsArray: TStrArray;
  const pos: Integer);
var
  len, I: Integer;
begin
  len := 0;
  constsArray := nil;
  I := pos + 1;
  while true do
  begin
    if (FLexems[I].LexType = 0) and (not IsReserved(FLexems[I].Lexem)) then
    begin
      Inc(len);
      SetLength(constsArray, len);
      constsArray[len - 1] := FLexems[I].Lexem;
      while FLexems[I].Lexem <> ';' do
        Inc(I);
      Inc(I);
    end
    else
      break;
  end;
end;

function TObfuscator.FindLexem(const Name: string;
  const Offset: Cardinal = 0): Cardinal;
var
  I: Integer;
begin
  Result := -1;
  for I := Offset to FLC - 1 do
  begin

    case StringQuote of
      sqSingle:
        begin
          if (I + 1) < FLC then
            if FLexems[I + 1].Lexem = '''' then
              Continue;
        end;
      sqDouble:
        begin
          if (I + 1) < FLC then
            if FLexems[I + 1].Lexem = '"' then
              Continue;
        end;
      sqSingleAndDouble:
        begin
          if (I + 1) < FLC then
            if (FLexems[I + 1].Lexem = '''') or (FLexems[I + 1].Lexem = '"')
            then
              Continue;
        end;
    end;

    if UpperCase(FLexems[I].Lexem) = UpperCase(Name) then
    begin
      Result := I;
      break;
    end;
  end;
end;

procedure TObfuscator.FindVars(var varsArray: TStrArray;
  const apos: Integer);
var
  len, I: Integer;
begin
  len := 0;
  varsArray := nil;
  I := apos + 1;
  while true do
  begin
    if (FLexems[I].LexType = 0) and (not IsReserved(FLexems[I].Lexem)) then
    begin
      Inc(len);
      SetLength(varsArray, len);
      varsArray[len - 1] := FLexems[I].Lexem;
      if FLexems[I + 1].Lexem = ',' then
      begin
        Inc(I, 2);
        while FLexems[I - 1].Lexem <> ':' do
        begin
          Inc(len);
          SetLength(varsArray, len);
          varsArray[len - 1] := FLexems[I].Lexem;
          Inc(I, 2)
        end;
      end;
      while FLexems[I].Lexem <> ';' do
        Inc(I);
      Inc(I);
    end
    else
      break;
  end;
end;

function TObfuscator.GenerateExpr(const num: string): string;
var
  tmp, t1, t2, t3, m, rn: Integer;
begin
  Result := '';
  tmp := strtoint(num);
  Randomize;
  t1 := tmp - (Random(tmp) + 1);
  t2 := tmp - t1;
  Result := Result + inttostr(t1) + '+';
  rn := Random(5) + 1;
  t3 := t2 div rn;
  m := t2 mod rn;
  Result := Result + '(' + inttostr(t3) + '*' + inttostr(rn) + '+' +
    inttostr(m) + ')';
end;

{function TObfuscator.GenerateInvslbName: string;
const
  InvslbSymbols: array [0 .. 6] of Char = (' ', '͖', 'ֹ', 'ٴ', '‎', '‏', ' ');
var
  len, I: Integer;
  tmpName: string;
begin
  tmpName := '';
  Randomize;
  len := Random(50);
  for I := 0 to len - 1 do
    tmpName := tmpName + InvslbSymbols[Random(7)];
  Result := tmpName;
end; }

function TObfuscator.GenerateUnRdblName: string;
const
  UnrdlbSymbols = 'qwertyuiopasdfghjklzxcvbnm_QWERTYUIOPASDFGHJKLZXCVBNM';
var
   S: string;
   i, N,len: integer;
begin
  len:= RandomRange(5,Length(UnrdlbSymbols));
  for i := 1 to len-1 do begin
    N := Random(Length(UnrdlbSymbols)) + 1;
    S := S + UnrdlbSymbols[N];
  end;
  Result:=S;
end;

procedure TObfuscator.GetGlobal;
var
  cPos, vPos: Integer;
  tmp: TStrArray;
begin
  cPos := 0;
  cPos := FindLexem('const', cPos + 1);
  while cPos > 0 do
  begin
    tmp := nil;
    FindConsts(tmp, cPos);
    GlobalConsts := GlobalConsts + TIdArray(tmp);
    cPos := FindLexem('const', cPos + 1);
  end;
  vPos := 0;
  vPos := FindLexem('var', vPos + 1);
  while vPos > 0 do
  begin
    tmp := nil;
    FindVars(tmp, vPos);
    GlobalVars := GlobalVars + TIdArray(tmp);
    vPos := FindLexem('var', vPos + 1);
  end;
end;

procedure TObfuscator.GetMethods;
var
  mPos, br, id: Integer;
  tmpN: string;
  tmpP: TBeginEnd;
begin
  mPos := 0;
  mPos := FindLexem('function', mPos + 1);
  while mPos > 0 do
  begin
    tmpN := FLexems[mPos + 1].Lexem;
    tmpP.beginPos := FindLexem('begin', mPos);
    br := 1;
    id := tmpP.beginPos + 1;
    while br > 0 do
    begin
      if UpperCase(FLexems[id].Lexem) = 'BEGIN' then
        Inc(br);
      if UpperCase(FLexems[id].Lexem) = 'END' then
        Dec(br);
      Inc(id);
    end;
    tmpP.endPos := id - 1;
    Methods := Methods + MArray(tmpN, tmpP);

    mPos := FindLexem('function', mPos + 1);
  end;


  mPos := FindLexem('procedure', mPos + 1);
  while mPos > 0 do
  begin
    tmpN := FLexems[mPos + 1].Lexem;
    tmpP.beginPos := FindLexem('begin', mPos);
    br := 1;
    id := tmpP.beginPos + 1;
    while br > 0 do
    begin
      if UpperCase(FLexems[id].Lexem) = 'BEGIN' then
        Inc(br);
      if UpperCase(FLexems[id].Lexem) = 'END' then
        Dec(br);
      Inc(id);
    end;
    tmpP.endPos := id - 1;
    Methods := Methods + MArray(tmpN, tmpP);

    mPos := FindLexem('procedure', mPos + 1);
  end;
end;

function TObfuscator.IsReserved(const Name: string): boolean;
var
  I: Integer;
begin
  Result := false;
  for I := 0 to 30 do
  begin
    if LowerCase(Name) = Reserved[I] then
    begin
      Result := true;
      Exit;
    end;
  end;
end;

procedure TObfuscator.Obfuscate;
var
  I: integer;
begin
  SetInText(FInnerText);
  GetGlobal;
  GetMethods;
  ObfuscateMethods;
  ObfuscateGlobals;
  if ooMuchBrackets in SetOptions then
    ObfuscateNumbers;

  FOutText := FLexems[0].Lexem;

  for I := 1 to FLC - 1 do
  begin
    if (FLexems[I - 1].LexType = 0) and ((FLexems[I].LexType = 0)) then
      FOutText := FOutText + ' ' + FLexems[I].Lexem
    else if (FLexems[I - 1].LexType = 0) and (FLexems[I].LexType = 1) then
    begin
      FOutText := FOutText + ' ' + FLexems[I].Lexem
    end
    else if (FLexems[I - 1].LexType = 1) and (FLexems[I].LexType = 0) then
    begin
      FOutText := FOutText + FLexems[I].Lexem
    end
    else
      FOutText := FOutText + FLexems[I].Lexem
  end;
end;

procedure TObfuscator.ObfuscateGlobals;
var
  I, j: Integer;
  atmp: string;
begin
  if Length(GlobalConsts.I) > 0 then
    for j := 0 to Length(GlobalConsts.I) - 1 do
    begin
      // if ooUnreadable in SetOptions then
       atmp := GenerateUnRdblName;
      for I := 2 to FLC - 1 do
      begin
        if UpperCase(FLexems[I].Lexem) = UpperCase(GlobalConsts.I[j]) then
          FLexems[I].Lexem := atmp;
      end;
    end;

  if Length(GlobalVars.I) > 0 then
    for j := 0 to Length(GlobalVars.I) - 1 do
    begin
      // if ooUnreadable in SetOptions then
       atmp := GenerateUnRdblName;
      for I := 2 to FLC - 1 do
      begin
        if (UpperCase(FLexems[I].Lexem) = UpperCase(GlobalVars.I[j])) and (FLexems[I-1].Lexem<>'.') then
          FLexems[I].Lexem := atmp;
      end;
    end;
end;

procedure TObfuscator.ObfuscateMethods;
var
  I, j: Integer;
  atmp: string;
begin
  if Length(Methods.Names) <= 0 then
    Exit;
  for j := 0 to Length(Methods.Names) - 1 do
  begin
    // if ooUnreadable in SetOptions then
     atmp := GenerateUnRdblName;
    for I := 2 to FLC - 1 do
    begin
      if UpperCase(FLexems[I].Lexem) = UpperCase(Methods.Names[j]) then
        FLexems[I].Lexem := atmp;
    end;
  end;
end;

procedure TObfuscator.ObfuscateNumbers;
var
  I: Integer;
begin
  for I := 0 to FLC - 1 do
  begin
    if FLexems[I].LexType = 1 then
    begin
      if pos('.', FLexems[I].Lexem) = 0 then
        FLexems[I].Lexem := GenerateExpr(FLexems[I].Lexem);
      FLexems[I].Lexem := AddBrackets(FLexems[I].Lexem);
    end;
  end;
end;

procedure TObfuscator.SetInText(txt: string);
var
  I: Integer;
begin
  FAnalyzer.Source := txt;
  FInnerText := FAnalyzer.Source;
  FAnalyzer.Analyze;
  SetLength(FLexems, FAnalyzer.LexemsCount);
  FLC := FAnalyzer.LexemsCount;
  for I := 0 to FAnalyzer.LexemsCount - 1 do
    FLexems[I] := FAnalyzer.Lexem[I]
end;

end.

