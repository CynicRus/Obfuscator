program PSObfuscator;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {Form1},
  Analyzer in 'Analyzer.pas',
  Obfuscator in 'Obfuscator.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
