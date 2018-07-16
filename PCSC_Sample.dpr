program PCSC_Sample;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  PcscDef in 'PcscDef.pas',
  PCSCRaw in 'PCSCRaw.pas',
  Reader in 'Reader.pas',
  OMSReader in 'OMSReader.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'PC/SC Sample Application V1.0';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
