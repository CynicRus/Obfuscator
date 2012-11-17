unit Obfuscator;
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
  Analyzer, StrUtils, SysUtils;

const
{$WARNINGS OFF}
  Chars: set of Char = ['(', ')', '+', '-', '*', '/', ';', '=', ')', '[', ']',
    ',', '@', ':', '.', '<', '>', '#', '$', '''', '"', '{'];
{$WARNINGS ON}
  Reserved: array [0 .. 27] of string = ('program', 'var', 'const', 'begin',
    'end', 'if', 'then', 'while', 'do', 'repeat', 'until', 'case', 'of', 'for',
    'to', 'as', 'is', 'mod', 'div', 'or', 'and', 'xor', 'uses', 'type',
    'function', 'procedure', 'string', 'nil');

type
  ObfuscationOptions = // Опции обфускации
    ( //
    ooUnreadable, // нечитаемые имена
    // ooInvisible, // невидимые имена , будут, если будет юникод
    ooMuchBrackets // вставка скобок вокруг числовых констант
    );

const
  SetOptions: // Выбранные опции
    set of ObfuscationOptions = [ooUnreadable, ooMuchBrackets];

type
  // TArray<t> = array of t;

  TBeginEnd = record
    beginPos, endPos: Integer;
  end;
  (*
    Тип для передачи тела методов обрабатывающим функциям
  *)

  TIdArray = record
    I: TArray<string>;
    class operator Implicit(a: TArray<string>): TIdArray;
    class operator Add(left, right: TIdArray): TIdArray;
  end;
  (*
    Массив идентификаторов ( константы и переменные )
  *)

  TMArray = record
    Names: TArray<string>;
    Positions: TArray<TBeginEnd>;
    class operator Add(left, right: TMArray): TMArray;
  end;
  (*
    Массив имен методов и их определений в коде
  *)

  TObfuscator = class(TObject)
  private
    FLexems: TLexems; // Массив лексем
    FLC: UInt32; // Кол-во лексем
    FOutText: string;
    FInnerText: string;
    GlobalConsts: TIdArray; // массив объявленных констант
    GlobalVars: TIdArray; // массив объявленныз переменных
    Methods: TMArray; // массив объявленных методов
    FAnalyzer: TLexicalAnalyzer; // лексический анализатор
    procedure FindConsts(var constsArray: TArray<string>; const pos: Integer);
    // Ищет имена констант, pos - индекс лексемы с именем 'conts'
    procedure FindVars(var varsArray: TArray<string>; const pos: Integer);
    // Ищет имена переменных, pos - индекс лексемы с именем 'var'
    procedure ObfuscateNumbers; //
    // добавляет скобки к числовым константам
    procedure ObfuscateMethods;
    // коверкает имена методов, и коверкаих их вызовы
    // в коде
    procedure ObfuscateGlobals;
    // то же, что и предыдущая процедура, но работает
    // с переменными и константами
    procedure GetGlobal; //
    // Ищет все объявления констант и переменных
    procedure GetMethods; //
    // Ищет все объявления методов
    function GenerateUnRdblName: string; //
    // Генерирует нечитаемое имя
{$HINTS OFF} // убрать директивы после включения данной ф-ии в работу
    function GenerateInvslbName: string; //
    // Генерирует невидимое имя
{$HINTS ON}
    function AddBrackets(const Src: string): string; //
    // Добавляет скобки вокруг идентификатора Src
    function FindLexem(const Name: string; const Offset: UInt32 = 0): Int32; //
    // Возвращает индекс лексемы с именем Name. Поиск начинается с индекса Offset
    function IsReserved(const Name: string): boolean;
    // Проверяет, является ли Name зарезервированным словом
    // Сеттеры
    procedure SetInText(txt: string);
  public
    procedure Obfuscate; // Обфусцирует входные данные
    constructor Create;
    destructor Destroy; override;
    property OutText: string read FOutText;
    // Обфусцированный скрипт - выход программы
    property InnerText: string read FInnerText write SetInText;
    // Входной текст. Должен быть синтаксически правильным.
  end;

function MArray(Name: string; Position: TBeginEnd): TMArray;
// Преобразует данные в тип TMArray

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

{ TObfuscator }

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

procedure TObfuscator.FindConsts(var constsArray: TArray<string>;
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
  const Offset: UInt32 = 0): Int32;
var
  I: Integer;
begin
  Result := -1;
  for I := Offset to FLC - 1 do
  begin

    case StringQuote of // Строковые константы не трогаем
      sqSingle:
        begin
          if FLexems[I + 1].Lexem =#39 then
            Continue;
        end;
      sqDouble:
        begin
          if FLexems[I + 1].Lexem = '"' then
            Continue;
        end;
      sqSingleAndDouble:
        begin
          if (FLexems[I + 1].Lexem = #39) or (FLexems[I + 1].Lexem = '"') then
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

procedure TObfuscator.FindVars(var varsArray: TArray<string>;
  const pos: Integer);
var
  len, I: Integer;
begin
  len := 0;
  varsArray := nil;
  I := pos + 1;
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

function TObfuscator.GenerateInvslbName: string;
const
  InvslbSymbols: array [0 .. 6] of Char = (' ', '?', '?', '?', '?', '?', ' ');
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
end;

function TObfuscator.GenerateUnRdblName: string;
const
  UnrdlbSymbols = 'qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM';
var
  len, I: Integer;
  tmpName: string;
begin
  tmpName := '';
  Randomize;
  len := 20+Random(25);
  for I := 0 to len - 1 do
    tmpName := tmpName + UnrdlbSymbols[Random(Length(UnrdlbSymbols)) + 1];
  Result := tmpName;
end;

procedure TObfuscator.GetGlobal;
var
  cPos, vPos: Integer;
  tmp: TArray<string>;
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
  // ищем функции
  mPos := FindLexem('function', mPos + 1);
  while mPos > 0 do
  begin
    tmpN := FLexems[mPos + 1].Lexem; // Получили имя метода
    tmpP.beginPos := FindLexem('begin', mPos); // позиция первого BEGIN
    br := 1; // кол-во открытых блоков begin..end
    id := tmpP.beginPos + 1;
    while br > 0 do
    begin
      if UpperCase(FLexems[id].Lexem) = 'BEGIN' then
        Inc(br);
      if UpperCase(FLexems[id].Lexem) = 'END' then
        Dec(br);
      Inc(id);
    end;
    tmpP.endPos := id - 1; // позиция последнего END
    Methods := Methods + MArray(tmpN, tmpP);

    mPos := FindLexem('function', mPos + 1);
  end;

  // процедуры
  mPos := FindLexem('procedure', mPos + 1);
  while mPos > 0 do
  begin
    tmpN := FLexems[mPos + 1].Lexem; // Получили имя метода
    tmpP.beginPos := FindLexem('begin', mPos); // позиция первого BEGIN
    br := 1; // кол-во открытых блоков begin..end
    id := tmpP.beginPos + 1;
    while br > 0 do
    begin
      if UpperCase(FLexems[id].Lexem) = 'BEGIN' then
        Inc(br);
      if UpperCase(FLexems[id].Lexem) = 'END' then
        Dec(br);
      Inc(id);
    end;
    tmpP.endPos := id - 1; // позиция последнего END
    Methods := Methods + MArray(tmpN, tmpP);

    mPos := FindLexem('procedure', mPos + 1);
  end;
end;

function TObfuscator.IsReserved(const Name: string): boolean;
var
  I: Integer;
begin
  Result := false;
  for I := 0 to 27 do
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
  I: Int32;
begin
  GetGlobal;
  GetMethods;
  ObfuscateMethods;
  ObfuscateGlobals;
  if ooMuchBrackets in SetOptions then
    ObfuscateNumbers;

  FOutText := FLexems[0].Lexem;
  (*
    Складываем лексемы вместе. В зависимости от типов стоящих рядом лексем,
    пробел между ними либо добавляется, либо нет. Например, можно написать
    a:=b+c; вместо a := b + c;, но нельзя слить вместе идентификаторы, например
    написать beginend. вместо begin end.
  *)
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
  tmp: string;
begin
  if Length(GlobalConsts.I) > 0 then
    for j := 0 to Length(GlobalConsts.I) - 1 do
    begin
      // if ooUnreadable in SetOptions then
      (*
        Раскомментируйте эту строку, когда будет поддержка Юникода. Тогда надо будет
        проверять, какие имена генерировать, невидимые или нечитаемые
      *)
      tmp := GenerateUnRdblName; { GeneraetInvsblName; }
    // tmp:='';
      for I := 2 to FLC - 1 do
      begin
        if UpperCase(FLexems[I].Lexem) = UpperCase(GlobalConsts.I[j]) then
          FLexems[I].Lexem := tmp;
      end;
    end;

  if Length(GlobalVars.I) > 0 then
    for j := 0 to Length(GlobalVars.I) - 1 do
    begin
      // if ooUnreadable in SetOptions then
      (*
        Раскомментируйте эту строку, когда будет поддержка Юникода. Тогда надо будет
        проверять, какие имена генерировать, невидимые или нечитаемые
      *)
      tmp := GenerateUnRdblName; { GeneraetInvsblName; }
      for I := 2 to FLC - 1 do
      begin
        if UpperCase(FLexems[I].Lexem) = UpperCase(GlobalVars.I[j]) then
          FLexems[I].Lexem := tmp;
      end;
    end;
end;

procedure TObfuscator.ObfuscateMethods;
var
  I, j: Integer;
  tmp: string;
begin
  if Length(Methods.Names) <= 0 then
    Exit;
  for j := 0 to Length(Methods.Names) - 1 do
  begin
    // if ooUnreadable in SetOptions then
    (*
      Раскомментируйте эту строку, когда будет поддержка Юникода. Тогда надо будет
      проверять, какие имена генерировать, невидимые или нечитаемые
    *)
    tmp := GenerateUnRdblName; { GeneraetInvsblName; }
    for I := 2 to FLC - 1 do
    begin
      if UpperCase(FLexems[I].Lexem) = UpperCase(Methods.Names[j]) then
        FLexems[I].Lexem := tmp;
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
      FLexems[I].Lexem := AddBrackets(FLexems[I].Lexem);
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

{ TIdArray }

class operator TIdArray.Add(left, right: TIdArray): TIdArray;
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

class operator TIdArray.Implicit(a: TArray<string>): TIdArray;
var
  j: Integer;
  t: TIdArray;
begin
  SetLength(t.I, Length(a));
  for j := 0 to Length(a) - 1 do
    t.I[j] := a[j];
  Result := t;
end;

{ TMArray }

class operator TMArray.Add(left, right: TMArray): TMArray;
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

end.
