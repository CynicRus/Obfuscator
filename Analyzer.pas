unit Analyzer;
/**
* This file is part of the Simba Obfuscator.
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
*/
interface

uses
  StrUtils, SysUtils;

const
{$WARNINGS OFF}
  IdentifierFirstSymbols: // ������ ���������� ������ ��������������
    set of Char = ['A' .. 'Z', 'a' .. 'z', '_'];
  IdentifierSymbols: // �������, ���������� � ��������������
    set of Char = ['A' .. 'Z', 'a' .. 'z', '0' .. '9', '_'];
  Digits: // �����
    set of Char = ['0' .. '9'];
  HexDigits: // ����������������� �����
    set of Char = ['A' .. 'F', 'a' .. 'f', '0' .. '9'];
  ExponentDot = '.'; // ���������� �����
  ExponentE: // ������, ���������� ���������� �� ��������
    set of Char = ['E', 'e'];
  Signs: set of Char = ['+', '-']; // ����� ����������
{$WARNINGS ON}
  CharLineEnd = ';'; // ������ ����� ����������
  CommentsLine = '//';
  CommentsOpen = '{';
  CommentsClose = '}';

type
  TStringQuote = // ���� �������, ������������ ��������� ���������
    ( //
    sqSingle, // ���������
    sqDouble, // �������
    sqSingleAndDouble // � ��, � ������
    );

const
  StringQuote = sqSingle; // ��� ������� �������� ���������

type
  PLexem = ^TLexem;

  TLexem = record // ��� ����� �������
    Lexem: string; // Ÿ ���
    TextPos: UInt32; // � ������� � ������
    LexType: Byte; // 0 - �������������; 1 - �����; 2 - ��� ���������
  end;

  TLexems = TArray<TLexem>; // ������ ������

  (*
    ����� ����������� ����� ������������ �����������. ��������� (CurrPos)
    ������������� �� ���� ����������. ��� ������ ���������� ���������
    �������: ������� ����������� ������� ������ (CurrChar), � ������ �����, �
    ����� ������� �� �����������: ���� � �������� ���������, ����������
    ��������� GetNumber, � �� ������ ���������� �������� ���������, ���������
    ������������ �� ������� �� ���� ����������; ���� ��� �������������, ��
    ���������� ������� GetIdentifier, ������� �������� �������������. �
    ��������� ������, ���������� ����� GetOthers, ������� �������� ��� ���������
    �������.
  *)

  TLexicalAnalyzer = class(TObject)
  private
    CurrPos: UInt32; // ��� �������� ������� ���������
    CurrChar: Char; // � ��� ������� �������������� ������
    CurrLexem: string; // ������� �������
    FLexemsList: TLexems; // ������ ������
    FLexemsCount: UInt32; // ���-�� ������
    FSource: string;

    procedure GetNextChar; // ��������� �� ��������� ������
    procedure Add(Lexem: string; Pos: UInt32; LType: Byte);
    // ��������� ������� � ������
    procedure GetNumber; // �������� �����
    procedure GetIdentifier; // �������� �������������
    procedure GetOthers; // �������� ��������� �������
    function NextChar: Char;
    // ���������� ��������� ������, �� ������� ���������

    function GetLexem(ind: UInt32): TLexem;
    procedure SetSource(src: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Analyze; // ���������� ������
    property Lexem[ind: UInt32]: TLexem read GetLexem;
    property LexemsCount: UInt32 read FLexemsCount;
    property Source: string read FSource write SetSource;
  end;

implementation

{ TLexicalAnalyzer }

procedure TLexicalAnalyzer.Add(Lexem: string; Pos: UInt32; LType: Byte);
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
    if CurrPos > Cardinal(Length(FSource)) then
      Break;
    case CurrChar of
      'A' .. 'Z', 'a' .. 'z', '_':
        begin
          CurrLexem := '';
          GetIdentifier;
          ctype := 0;
          if CurrLexem <> '' then
            Add(CurrLexem, CurrPos - Cardinal(Length(CurrLexem)), ctype);
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

function TLexicalAnalyzer.GetLexem(ind: UInt32): TLexem;
begin
  if ind > (FLexemsCount - 1) then
    Exit;
  Result := FLexemsList[ind];
end;

procedure TLexicalAnalyzer.GetNextChar;
begin
  Inc(CurrPos);
  if CurrPos > Cardinal(Length(FSource)) then
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
    Exit; // ����� �����

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
    Exit; // ������������ ����� ��� �������� ����������

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
  // ����� :)
end;

procedure TLexicalAnalyzer.GetOthers;
begin
  case CurrChar of
    '+', '-', '*', '/', ';', '=', ')', '[', ']', ',', '@':
      // �������, ������� ���������� ����� ������� � ����� �������
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
          Add('''', CurrPos, 2);
          GetNextChar;
          while CurrChar <> '''' do
          begin
            CurrLexem := CurrLexem + CurrChar;
            GetNextChar;
          end;
          if CurrLexem <> '' then
            Add(CurrLexem, CurrPos - Cardinal(Length(CurrLexem)), 2);
          CurrLexem := '''';
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
    Exit; // ���� ����������� ������. �� ���� ������� ���������� ������,
    // ���� �� ������ �� ���� ������, �� �������� ����� ��
    // �������� �������
  end;
end;

function TLexicalAnalyzer.NextChar: Char;
begin
  if CurrPos + 1 > Cardinal(Length(FSource)) then
    Result := #0
  else
    Result := FSource[CurrPos + 1];
end;

procedure TLexicalAnalyzer.SetSource(src: String);
var
  i, j: Integer;
  tmp: string;
begin
  try
    tmp := src;

    // ������� �����������
    // ������� ������������
    j := PosEx(CommentsLine, tmp, 1);
    while j > 0 do
    begin
      i := j + 2;
      while (tmp[i] <> #10) and (i <= Length(tmp)) do
        Inc(i);
      Delete(tmp, j, i - j);
      j := PosEx(CommentsLine, tmp, 1);
    end;

    // ������� ������� � ������ � ����� ������
    i := 1;
    while CharInSet(tmp[i], [' ', #00 .. #13]) do
      Inc(i);
    Delete(tmp, 1, i - 1);
    i := Length(tmp);
    while CharInSet(tmp[i], [' ', #00 .. #13]) do
      Dec(i);
    Delete(tmp, i + 1, Length(tmp) - i + 1);
  finally
    Self.FSource := tmp;
    tmp := '';
  end;
end;

end.
