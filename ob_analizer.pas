unit ob_analizer;
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
  StrUtils, SysUtils;
const
{$WARNINGS OFF}
  IdentifierFirstSymbols:
    set of Char = ['A' .. 'Z', 'a' .. 'z', '_'];
  IdentifierSymbols:
    set of Char = ['A' .. 'Z', 'a' .. 'z', '0' .. '9', '_'];
  Digits:
    set of Char = ['0' .. '9'];
  HexDigits:
    set of Char = ['A' .. 'F', 'a' .. 'f', '0' .. '9'];
  ExponentDot = '.';
  ExponentE:
    set of Char = ['E', 'e'];
  Signs: set of Char = ['+', '-'];
{$WARNINGS ON}
  CharLineEnd = ';';
  CommentsLine = '//';
  CommentsOpen = '{';
  CommentsClose = '}';

  type
  TStringQuote =
    (
    sqSingle,
    sqDouble,
    sqSingleAndDouble
    );

const
  StringQuote = sqSingle;

type

  TCharSet = set of Char;

  PLexem = ^TLexem;

  TLexem = record
    Lexem: string;
    TextPos: integer;
    LexType: Byte;
  end;
  TLexems = array of TLexem;

  TLexicalAnalyzer = class(TObject)
  private
    CurrPos: Integer;
    CurrChar: Char;
    CurrLexem: string;
    FLexemsList: TLexems;
    FLexemsCount: Integer;
    FSource: string;

    procedure GetNextChar;
    procedure Add(Lexem: string; Pos: Integer; LType: Byte);
    procedure GetNumber;
    procedure GetIdentifier;
    procedure GetOthers;
    function NextChar: Char;
   { function GenerateAnsi(const Str: string): string;}
    function GetLexem(ind: Integer): TLexem;
    procedure SetSource(src: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Analyze;
    property Lexem[ind: Integer]: TLexem read GetLexem;
    property LexemsCount: Integer read FLexemsCount;
    property Source: string read FSource write SetSource;
  end;

implementation
{Small helper}
function Eq(aValue1, aValue2: string): boolean;
//--------------------------------------------------------
begin
  Result := AnsiCompareText(Trim(aValue1),Trim(aValue2))=0;
end;

procedure DeleteSpaces(var S: String);
var
  I, C: Integer;
begin
  I := 1;
  C := 0;
  while I <= Length(S) do
  begin
    if S[I] = #9 then S[I] := #32;
    if S[I] = #32 then Inc(C) else
      if C <> 0 then
      begin
        if C > 1 then Delete(S, I - C, C - 1);
        C := 0;
        Dec(I, C - 2);
      end;
    Inc(I);
  end;
end;

function CharInSet(const Ch: Char; const Chars: TCharSet): Boolean;
begin
  Result := Ch in Chars;
end;

function GenerateAnsi(const Str: string): string;
var
  I: Integer;
  s: string;
begin
  s := '';
  for I := 1 to Length(Str) do
    if not (Ord(Str[I]) = 39) then
    s := s + '#' + inttostr(Ord(Str[I]));
    Result:=s;
end;

function PrepareLongLines(const Str: string): string;
var
  i,j,ch: integer;
  tmp: string;
begin
  tmp:=str;
  DeleteSpaces(tmp);
  j:=Length(tmp);
  for I:=0 to j do
    begin
       Ch:=pos('+',tmp);
       if (tmp[Ch-1] = #39) or (tmp[Ch+1] = #39) then
       Delete(tmp,Ch,1);
      end;
  result:=tmp;
  end;
function PrepareStrValue(const Str: string): string;
var
  i,j,ch: integer;
  tmp: string;
begin
  tmp:=str;
  j:=Length(Str);
  tmp:=PrepareLongLines(tmp);
  for I:=0 to j do
    begin
       Ch:=pos(#39,tmp);
       Delete(tmp,Ch,1);
      end;
  if not eq(tmp,'') then
   result:=GenerateAnsi(tmp)
   else
   result:=#39+#39;
  end;

{ TLexicalAnalyzer }

procedure TLexicalAnalyzer.Add(Lexem: string; Pos: Integer; LType: Byte);
begin
  if Lexem = '' then
    Exit;
  Inc(FLexemsCount);
  SetLength(FLexemsList, FLexemsCount);
  FLexemsList[FLexemsCount - 1].Lexem := Lexem;
  FLexemsList[FLexemsCount - 1].TextPos := Pos;
  FLexemsList[FLexemsCount - 1].LexType := LType;
end;

procedure TLexicalAnalyzer.Analyze;
var
  ctype: Byte;
begin
  SetLength(FLexemsList, 0);
  GetNextChar;
  while CurrChar <> #0 do
  begin
    if CurrPos > Length(FSource) then
      Break;
    case CurrChar of
      'A' .. 'Z', 'a' .. 'z', '_':
        begin
          CurrLexem := '';
          GetIdentifier;
          ctype := 0;
          if CurrLexem <> '' then
            Add(CurrLexem, CurrPos - Length(CurrLexem), ctype);
          CurrLexem := '';
        end;
      '0' .. '9':
        begin
          CurrLexem := '';
          GetNumber;
          ctype := 1;
          if CurrLexem <> '' then
            Add(CurrLexem, CurrPos - Cardinal(Length(CurrLexem)), ctype);
          CurrLexem := '';
        end;
      #1 .. #20, ' ':
        begin
          GetNextChar;
        end;
    else
      begin
        CurrLexem := '';
        GetOthers;
        ctype := 2;
        if CurrLexem <> '' then
          Add(CurrLexem, CurrPos - Cardinal(Length(CurrLexem)), ctype);
        CurrLexem := '';
      end;
    end;
  end;
end;

constructor TLexicalAnalyzer.Create;
begin
  inherited;
  FLexemsList := nil;
  FLexemsCount := 0;
  CurrPos := 0;
  CurrChar := #0;
  CurrLexem := '';
end;

destructor TLexicalAnalyzer.Destroy;
begin
  SetLength(FLexemsList, 0);
  inherited;
end;

procedure TLexicalAnalyzer.GetIdentifier;
begin
  CurrLexem := CurrChar;
  GetNextChar;
  while CharInSet(CurrChar, IdentifierSymbols) do
  begin
    CurrLexem := CurrLexem + CurrChar;
    GetNextChar;
  end;
end;

function TLexicalAnalyzer.GetLexem(ind: integer): TLexem;
begin
  if ind > (FLexemsCount - 1) then
    Exit;
  Result := FLexemsList[ind];
end;

procedure TLexicalAnalyzer.GetNextChar;
begin
  Inc(CurrPos);
  if CurrPos > Length(FSource) then
    CurrChar := #0
  else
    CurrChar := FSource[CurrPos];
end;

procedure TLexicalAnalyzer.GetNumber;
begin
  CurrLexem := CurrChar;
  GetNextChar;

  while CharInSet(CurrChar, Digits) do
  begin
    CurrLexem := CurrLexem + CurrChar;
    GetNextChar;
  end;
  if CurrChar <> ExponentDot then
    Exit;

  if not CharInSet(NextChar, Digits) then
    Exit;

  CurrLexem := CurrLexem + ExponentDot;
  GetNextChar;

  while CharInSet(CurrChar, Digits) do
  begin
    CurrLexem := CurrLexem + CurrChar;
    GetNextChar;
  end;

  if not CharInSet(CurrChar, ExponentE) then
    Exit;

  CurrLexem := CurrLexem + 'E';
  GetNextChar;

  if CharInSet(CurrChar, Signs) then
    CurrLexem := CurrLexem + CurrChar;
  GetNextChar;

  while CharInSet(CurrChar, Digits) do
  begin
    CurrLexem := CurrLexem + CurrChar;
    GetNextChar;
  end;

end;

procedure TLexicalAnalyzer.GetOthers;
var
  i: integer;
begin
  case CurrChar of
    '+', '-', '*', '/', ';', '=', ')', '[', ']', ',', '@':
      begin
        CurrLexem := CurrChar;
        GetNextChar;
        Exit;
      end;
    ':':
      begin
        if NextChar = '=' then
        begin
          GetNextChar;
          GetNextChar;
          CurrLexem := ':=';
        end
        else
        begin
          GetNextChar;
          CurrLexem := ':';
        end;
        Exit;
      end;

    '.':
      begin
        if NextChar = '.' then
        begin
          GetNextChar;
          GetNextChar;
          CurrLexem := '..';
        end
        else if NextChar = ')' then
        begin
          GetNextChar;
          GetNextChar;
          CurrLexem := '.)';
        end
        else
        begin
          GetNextChar;
          CurrLexem := '.';
        end;
        Exit;
      end;
    '(':
      begin
        if NextChar = '.' then
        begin
          GetNextChar;
          GetNextChar;
          CurrLexem := '(.';
        end
        else
        begin
          GetNextChar;
          CurrLexem := '(';
        end;
        Exit;
      end;
    '<', '>':
      begin
        if NextChar = '=' then
        begin
          CurrLexem := CurrChar + '=';
          GetNextChar;
          GetNextChar;
        end
        else if NextChar = '>' then
        begin
          CurrLexem := CurrChar + '>';
          GetNextChar;
          GetNextChar;
        end
        else
        begin
          CurrLexem := CurrChar;
          GetNextChar;
        end;
        Exit;
      end;
    '#':
      begin
        GetNextChar;
        CurrLexem := '#';
        while CharInSet(CurrChar, Digits) do
        begin
          CurrLexem := CurrLexem + CurrChar;
          GetNextChar;
        end;
      end;
    '$':
      begin
        GetNextChar;
        CurrLexem := '$';
        while CharInSet(CurrChar, HexDigits) do
        begin
          CurrLexem := CurrLexem + CurrChar;
          GetNextChar;
        end;
      end;
    '''':
      begin
        if (StringQuote = sqSingle) or (StringQuote = sqSingleAndDouble) then
        begin
          // Add('''', CurrPos, 2);
          GetNextChar;
          while CurrChar <> '''' do
          begin
            CurrLexem := CurrLexem + CurrChar;
            GetNextChar;
          end;
          if (CurrLexem <> '') then
          begin
            CurrLexem := PrepareStrValue(CurrLexem);
            Add(CurrLexem, CurrPos - Cardinal(Length(CurrLexem)), 2);
            CurrLexem := '';
          end
          else
          begin
            CurrLexem := PrepareStrValue(#39+CurrLexem+#39);
            Add(CurrLexem, CurrPos - Cardinal(Length(CurrLexem)), 2);
            CurrLexem := '';
          end;
          // CurrLexem := '''';
          GetNextChar;
          { Add('''', CurrPos, 2);
            GetNextChar; }
        end;
      end;
    '"':
      begin
        if (StringQuote = sqDouble) or (StringQuote = sqSingleAndDouble) then
        begin
          Add('"', CurrPos, 2);
          GetNextChar;
          while CurrChar <> '"' do
          begin
            CurrLexem := CurrLexem + CurrChar;
            GetNextChar;
          end;
          if CurrLexem <> '' then
            Add(CurrLexem, CurrPos - Cardinal(Length(CurrLexem)), 2);
          CurrLexem := '"';
          GetNextChar;
          { Add('"', CurrPos, 2);
            GetNextChar; }
        end;
      end;
    '{':
      begin
        if NextChar = '$' then
        begin
          GetNextChar;
          CurrLexem := '{';
          while CurrChar <> '}' do
          begin
            CurrLexem := CurrLexem + CurrChar;
            GetNextChar;
          end;
          CurrLexem := CurrLexem + '}';
          GetNextChar;
        end
        else
        begin
          CurrLexem := '';
          while CurrChar <> '}' do
            GetNextChar;
          GetNextChar;
        end;
      end
  else
    Exit;
  end;
end;

function TLexicalAnalyzer.NextChar: Char;
begin
  if CurrPos + 1 > Length(FSource) then
    Result := #0
  else
    Result := FSource[CurrPos + 1];
end;

procedure TLexicalAnalyzer.SetSource(src: String);
var
  I, j: Integer;
  tmp: string;
begin
  try
    tmp := src;

    j := PosEx(CommentsLine, tmp, 1);
    while j > 0 do
    begin
      I := j + 2;
      while (tmp[I] <> #10) and (I <= Length(tmp)) do
        Inc(I);
      Delete(tmp, j, I - j);
      j := PosEx(CommentsLine, tmp, 1);
    end;


    I := 1;
    while CharInSet(tmp[I], [' ', #00 .. #13]) do
      Inc(I);
    Delete(tmp, 1, I - 1);
    I := Length(tmp);
    while CharInSet(tmp[I], [' ', #00 .. #13]) do
      Dec(I);
    Delete(tmp, I + 1, Length(tmp) - I + 1);
  finally
    Self.FSource := tmp;
    tmp := '';
  end;
end;

end.

