//////////////////////////////////////////////////////////////////////////////////
//  Copyright (C) 2008 SCM Microsystems Inc.                                    //
//                                                                              //
//     This source code is provided 'as-is', without any express or implied     //
//     warranty. In no event will SCM Microsystems Inc. be held liable for any  //
//     damages arising from the use of this software.                           //
//                                                                              //
//     SCM Microsystems Inc. does not warrant, that the source code will be     //
//     free from defects in design or workmanship or that operation of the      //
//     source code will be error-free. No implied or statutory warranty of      //
//     merchantability or fitness for a particulat purpose shall apply.         //
//     The entire risk of quality and performance is with the user of this      //
//     source code.                                                             //
//                                                                              //
//     Permission is granted to anyone to use this software for any purpose,    //
//     including commercial applications, and to alter it and redistribute it   //
//     freely, subject to the following restrictions:                           //
//                                                                              //
//     1. The origin of this source code must not be misrepresented; you must   //
//        not claim that you wrote the original source code. If you use this    //
//        source code in a product, an acknowledgment in the product            //
//        documentation would be appreciated but is not required.               //
//                                                                              //
//     2. Altered source versions must be plainly marked as such, and must not  //
//        be misrepresented as being the original source code.                  //
//                                                                              //
//     3. This notice may not be removed or altered from any source             //
//        distribution.                                                         //
//////////////////////////////////////////////////////////////////////////////////
                                                                                
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
  Caption:='Чтение карт ОМС';
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
      SL.Add('Номер полиса=' + OI.MedicalIncuranceNumber);
      SL.Add('Дата создания=' + Date2Str(OI.RegistrationDate));
      SL.Add('Действует до=' + Date2Str(OI.MedicalInsuranceDateEnd));
      SL.Add('Номер карточки=' + OI.Policy);
      SL.Add('Фамилия=' + OI.LName);
      SL.Add('Имя=' + OI.FName);
      SL.Add('Отчество=' + OI.MName);
      SL.Add('Пол=' + OI.SexName);
      SL.Add('Дата рождения=' + Date2Str(OI.Byrthday));
      SL.Add('Место рождения=' + OI.BirthPlace);
      SL.Add('Код страны=' + OI.CountryCode);
      SL.Add('Название страны=' + OI.Country);
      SL.Add('СНИЛС=' + OI.InsuranceCertificate);
      SL.Add('Формат изображения=' + OI.ImgFormat);
      if OI.Img <> '' then
        SL.Add('Данные фоторгафии=Есть')
      else
        SL.Add('Данные фоторгафии=');
      SL.Add('ОГРН для СМО=' + OI.OGRN);
      SL.Add('ОКАТО для СМО=' + OI.OKATO);
      SL.Add('Дата начала страхования в СМО=' + Date2Str(OI.DateBegin));
      SL.Add('Дата окончания страхования в СМО=' + Date2Str(OI.DateEnd));
    end
    else
    begin
      SL.Add('Состояние=Карточка отсутствует');
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

