unit MainUnit;
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
  SysUtils, Forms,Dialogs, StdCtrls, Obfuscator,
  SynEditHighlighter, SynHighlighterPas, SynEdit, Controls, Classes;

type
  TForm1 = class(TForm)
    Button1: TButton;
    SynEdit1: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    SynEdit2: TSynEdit;
    Label1: TLabel;
    Label2: TLabel;
    Button2: TButton;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  obf: TObfuscator;
  tbuf: string;
begin
  (Sender as TButton).Enabled := false;
  if SynEdit1.Text = '' then
    Exit;
  tbuf := '';
  obf := TObfuscator.Create;
  try
    try
      obf.InnerText := SynEdit1.Text;
      obf.Obfuscate;
      SynEdit2.Clear;
      SynEdit2.Text := obf.OutText;
    except
      on e: Exception do
        MessageDlg(e.ClassName + ': ' + e.Message, mtError, [mbOk], 0);
    end;
  finally
    obf.Destroy;
  end;
  (Sender as TButton).Enabled := true;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    SynEdit2.Lines.SaveToFile(SaveDialog1.FileName);
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    SynEdit1.Lines.LoadFromFile(OpenDialog1.FileName);
  end;
end;

end.
