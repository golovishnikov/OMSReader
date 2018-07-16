                                                                              
unit Main;

interface

uses
  Windows, SysUtils, Messages, Classes, Forms, Controls, ComCtrls, StdCtrls,
  ExtCtrls, Graphics, XPMan, PCSCRaw, PCSCDef, Reader, OMSReader, Grids,
  ValEdit;

type
  TMainForm = class(TForm)
    XPManifest1: TXPManifest;
    Memo1: TMemo;
    OwnerInfo: TValueListEditor;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FOmsReader: TOMSReader;
    procedure AddLog(Msg: String);
    procedure DoLog(LogType: TLogType; FunctionResult: DWORD; Msg: String);
    procedure OMSReaderSatetChange(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation


{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Caption:='������ ���� ���';
  Application.Title:=Caption;

  Memo1.Clear;
  OwnerInfo.Strings.Clear;

  FOmsReader := TOMSReader.Create(DoLog);
  FOmsReader.OnSatetChange := OMSReaderSatetChange;
  OMSReaderSatetChange(FOmsReader);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  i:integer;
begin
  FOmsReader.Free;
end;


procedure TMainForm.DoLog(LogType: TLogType; FunctionResult: DWORD; Msg: String);
var
  N: String;
begin
  case LogType of
    ltInit: N := 'ltInit';
    ltReaderListChange: N := 'ltReaderListChange';
    ltCardStateChange: N := 'ltCardStateChange';
    ltConnect: N := 'ltConnect';
    ltDisconnect: N := 'ltDisconnect';
    ltSend: N := 'ltSend';
    ltRecive: N := 'ltRecive';
    ltMessage: N := 'ltMessage';
    else N := 'unknow';
  end;
  AddLog('Event=' + N + ', Result=' + IntToStr(FunctionResult) + ', Msg=' + Msg);
end;

function Date2Str(D: TDate): String;
begin
  if D <> 0 then
    Result := FormatDateTime('dd.mm.yyyy', D)
  else
    Result := '';
end;

procedure TMainForm.OMSReaderSatetChange(Sender: TObject);
var
  OI: TOwnerInfo;
  SL: TStringList;
begin
  OwnerInfo.Strings.Clear;
  SL := TStringList.Create;
  try
    if (Sender as TOMSReader).ReadOwnerInfo(OI) then
    begin
      SL.Add('����� ������=' + OI.MedicalInsuranceNumber);
      SL.Add('���� ��������=' + Date2Str(OI.RegistrationDate));
      SL.Add('��������� ��=' + Date2Str(OI.MedicalInsuranceDateEnd));
      SL.Add('����� ��������=' + OI.Policy);
      SL.Add('�������=' + OI.LName);
      SL.Add('���=' + OI.FName);
      SL.Add('��������=' + OI.MName);
      SL.Add('���=' + OI.SexName);
      SL.Add('���� ��������=' + Date2Str(OI.Byrthday));
      SL.Add('����� ��������=' + OI.BirthPlace);
      SL.Add('��� ������=' + OI.CountryCode);
      SL.Add('�������� ������=' + OI.Country);
      SL.Add('�����=' + OI.InsuranceCertificate);
      SL.Add('������ �����������=' + OI.ImgFormat);
      if OI.Img <> '' then
        SL.Add('������ ����������=����')
      else
        SL.Add('������ ����������=');
      SL.Add('���� ��� ���=' + OI.OGRN);
      SL.Add('����� ��� ���=' + OI.OKATO);
      SL.Add('���� ������ ����������� � ���=' + Date2Str(OI.DateBegin));
      SL.Add('���� ��������� ����������� � ���=' + Date2Str(OI.DateEnd));
    end
    else
    begin
      SL.Add('���������=�������� �����������');
    end;
    if Assigned(OwnerInfo) then OwnerInfo.Strings.Assign(SL);
  finally
    SL.Free;
  end;
end;

procedure TMainForm.AddLog(Msg: String);
begin
  Memo1.Lines.Add(Msg);
  Memo1.Perform(EM_SCROLLCARET, 0, 0);
end;

end.

