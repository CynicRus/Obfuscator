unit ob_main;
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
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterPas, SynMemo, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnList: TImageList;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    StatusBar1: TStatusBar;
    CodeMemo: TSynEdit;
    CodeTab: TTabSheet;
    ProcCode: TTabSheet;
    SynEdit1: TSynEdit;
    ObfCode: TSynMemo;
    SynPasSyn1: TSynPasSyn;
    FormatCode: TTabSheet;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure ToolButton6Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation
 uses ob_obfuscator;
{$R *.lfm}

{ TForm1 }

procedure TForm1.ToolButton1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Self.Caption:='Obfuscator for Simba\SCAR'+#32+ 'v 0.2.1 for' +#32+ {$IFDEF WINDOWS}'WIN'{$ELSE}'LIN'{$ENDIF}+#32+ 'by Cynic';
end;

procedure TForm1.ToolButton3Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    CodeMemo.Lines.LoadFromFile(OpenDialog1.FileName);
  end;
end;

procedure TForm1.ToolButton6Click(Sender: TObject);
var
  obf: TObfuscator;
  tbuf: string;
begin
  (Sender as TToolButton).Enabled := false;
  if CodeMemo.Text = '' then
    Exit;
  tbuf := '';
  obf := TObfuscator.Create;
  try
    try
      obf.InnerText := CodeMemo.Text;
      obf.Obfuscate;
      obfCode.Lines.Clear;
      obfCode.Text := obf.OutText;
    except
      on e: Exception do
        MessageDlg(e.ClassName + ': ' + e.Message, mtError, [mbOk], 0);
    end;
  finally
    obf.Destroy;
  end;
  (Sender as TToolButton).Enabled := true;
end;

end.

