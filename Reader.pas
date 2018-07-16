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
                                                                                
unit Reader;

interface

uses
  Windows, SysUtils, Classes, PCSCRaw, PCSCDef;

type
  TReaderObject=class;
  TOnReaderListChanged = procedure of object;
  TCardState =(csUnknown=0, csExclusive, csShared, csAvailable, csBadCard, csNoCard);

  TReaderListThread = class(TThread)
  private
    FPCSCRaw:TPCSCRaw;
    FPCSCDeviceContext:DWORD;
    FReaderState:TSCardReaderStateA;
    FOnReaderListChanged:TOnReaderListChanged;
  public
    constructor Create(PCSCRaw:TPCSCRaw);
    procedure Execute; override;

    property OnReaderListChanged:TOnReaderListChanged read FOnReaderListChanged write FOnReaderListChanged;
  end;

  TCardStateThread = class(TThread)
  private
    FParentReader:TReaderObject;
    FReaderName:string;
    FPCSCRaw:TPCSCRaw;
    FPCSCDeviceContext:DWORD;
    FCardState:TCardState;
    FReaderStateArray:array[0..0]of TSCardReaderStateA;
  public
    constructor Create(ParentReader:TReaderObject;PCSCRaw:TPCSCRaw);
    procedure Execute; override;

    property CardState:TCardState read FCardState;
  end;

  TReaderObject=class
  private
    FOnCardStateChanged:TNotifyEvent;
    FCardStateThread:TCardStateThread;
    FPCSCRaw:TPCSCRaw;
    FSCardReaderState:TSCardReaderStateA;

    FReaderName:string;
    FATRString:string;
    FCardState:TCardState;

    FCardHandle:DWORD;
    FProtocolType: TPcscProtocol;

    procedure DecodeATR(ATR:PChar;ATRLen:integer);
    procedure CardStateChanged;
  public
    constructor Create(AReaderName:string;PCSCRaw:TPCSCRaw);
    destructor Destroy;override;

    function SCConnect(PCSCRaw:TPCSCRaw;DeviceContext,ShareMode:DWORD):DWORD;
    function SCDisconnect(PCSCRaw:TPCSCRaw;DeviceContext:DWORD):DWORD;
    function SCTransmit(PCSCRaw:TPCSCRaw;DeviceContext:DWORD;InBuffer,OutBuffer:Pointer;InSize:DWORD;var OutSize:DWORD):DWORD;

    property ATR:string read FATRString;
    property ReaderName:string read FReaderName;
    property CardState:TCardState read FCardState;

    property CardHandle:THandle read FCardHandle;
    property OnCardStateChanged:TNotifyEvent read FOnCardStateChanged write FOnCardStateChanged;
  end;

implementation

constructor TReaderListThread.Create(PCSCRaw:TPCSCRaw);
begin
  inherited Create(true);
  FPCSCRaw:=PCSCRaw;
  FPCSCDeviceContext:=0;
  FOnReaderListChanged:=nil;

  FPCSCRaw.SCardEstablishContext(SCARD_SCOPE_SYSTEM,nil,nil,FPCSCDeviceContext);
end;

procedure TReaderListThread.Execute;
var
  PCSCResult:DWORD;
begin
  FReaderState.cbAtr:=0;
  FReaderState.dwEventState:=SCARD_STATE_UNAWARE;
  FReaderState.dwCurrentState:=SCARD_STATE_UNAWARE;
  FReaderState.szReader:='\\?PNP?\Notification';
  FReaderState.pvUserData:=nil;

  while not Terminated do begin
    PCSCResult:=FPCSCRaw.SCardGetStatusChange(FPCSCDeviceContext,250,@FReaderState,1);
    if PCSCResult=SCARD_E_CANCELLED then break;
    if PCSCResult=SCARD_S_SUCCESS then begin
      FReaderState.dwCurrentState:=FReaderState.dwEventState;
      if Assigned(FOnReaderListChanged) then Synchronize(FOnReaderListChanged);
    end;
  end;
  if FPCSCDeviceContext<>0 then FPCSCRaw.SCardReleaseContext(FPCSCDeviceContext);
end;

constructor TCardStateThread.Create(ParentReader:TReaderObject;PCSCRaw:TPCSCRaw);
begin
  inherited Create(true);
  FParentReader:=ParentReader;
  FPCSCRaw:=PCSCRaw;
  if FParentReader<>nil then FReaderName:=FParentReader.ReaderName;
  FPCSCDeviceContext:=0;
  FCardState:=csUnknown;

  FPCSCRaw.SCardEstablishContext(SCARD_SCOPE_SYSTEM,nil,nil,FPCSCDeviceContext);
end;

procedure TCardStateThread.Execute;
var
  PCSCResult:DWORD;
begin
  FReaderStateArray[0].cbAtr:=0;
  FReaderStateArray[0].dwEventState:=SCARD_STATE_UNAWARE;
  FReaderStateArray[0].dwCurrentState:=SCARD_STATE_UNAWARE;
  FReaderStateArray[0].szReader:=@FReaderName[1];
  FReaderStateArray[0].pvUserData:=nil;

  while not Terminated do begin
    PCSCResult:=FPCSCRaw.SCardGetStatusChange(FPCSCDeviceContext,250,@FReaderStateArray,1);
    if PCSCResult=SCARD_E_CANCELLED then break;
    if PCSCResult=SCARD_S_SUCCESS then begin
      FReaderStateArray[0].dwCurrentState:=FReaderStateArray[0].dwEventState;
      FParentReader.FSCardReaderState:=FReaderStateArray[0];
      Synchronize(FParentReader.CardStateChanged);
    end;
  end;
  if FPCSCDeviceContext<>0 then FPCSCRaw.SCardReleaseContext(FPCSCDeviceContext);
end;

constructor TReaderObject.Create(AReaderName:string;PCSCRaw:TPCSCRaw);
begin
  FReaderName:=AReaderName;
  FOnCardStateChanged:=nil;
  FPCSCRaw:=PCSCRaw;
  FCardStateThread:=TCardStateThread.Create(self,PCSCRaw);

  FATRString:='';
  FCardState:=csUnknown;
  FCardHandle:=INVALID_HANDLE_VALUE;

  FCardStateThread.Resume;
end;

destructor TReaderObject.Destroy;
begin
  FCardStateThread.Terminate;
  FCardStateThread.Free;
  inherited;
end;

procedure TReaderObject.DecodeATR(ATR:PChar;ATRLen:integer);
var
  i:integer;
begin
  FATRString:='';
  for i:=0 to ATRLen-1 do FATRString:=FATRString+IntToHex(PByteArray(ATR)[i],2)+' ';
end;

function TReaderObject.SCConnect(PCSCRaw:TPCSCRaw;DeviceContext,ShareMode:DWORD):DWORD;
var
  actprot:DWORD;
  rname:shortstring;
begin
  result:=SCARD_S_SUCCESS;
  if FCardHandle<>INVALID_HANDLE_VALUE then exit;
  rname:=copy(FReaderName,1,254)+#0;
  if ShareMode=SCARD_SHARE_DIRECT then Result:=PCSCRaw.SCardConnect(DeviceContext, @rname[1], SCARD_SHARE_DIRECT, 0, FCardHandle, actprot)
  else Result:=PCSCRaw.SCardConnect(DeviceContext, @rname[1], ShareMode, SCARD_PROTOCOL_Tx, FCardHandle, actprot);
  if result=SCARD_S_SUCCESS then begin
    if actprot=SCARD_PROTOCOL_T0 then FProtocolType:=prT0
    else if actprot=SCARD_PROTOCOL_T1 then FProtocolType:=prT1
    else if actprot=SCARD_PROTOCOL_RAW then FProtocolType:=prRaw
    else FProtocolType:=prNC;
  end;
end;

function TReaderObject.SCDisconnect(PCSCRaw:TPCSCRaw;DeviceContext:DWORD):DWORD;
begin
  result:=SCARD_S_SUCCESS;
  if FCardHandle=INVALID_HANDLE_VALUE then exit;
  Result:=PCSCRaw.SCardDisconnect(FCardHandle,SCARD_UNPOWER_CARD);
  FProtocolType:=prNC;
  FCardHandle:=INVALID_HANDLE_VALUE;
end;

function TReaderObject.SCTransmit(PCSCRaw:TPCSCRaw;DeviceContext:DWORD;InBuffer,OutBuffer:Pointer;InSize:DWORD;var OutSize:DWORD):DWORD;
var
  pioSendPCI, pioRecvPCI: pSCardIORequest;
begin
  case FProtocolType of
    prT0: begin pioSendPCI:=@SCARDPCIT0; pioRecvPCI:=nil; end;
    prT1: begin pioSendPCI:=@SCARDPCIT1; pioRecvPCI:=nil; end;
    else begin
      result:=ERROR_INVALID_PARAMETER;
      exit;
    end;
  end;
  Result:=PCSCRaw.SCardTransmit(FCardHandle,pioSendPCI,InBuffer,InSize,pioRecvPCI,OutBuffer,OutSize);
end;

procedure TReaderObject.CardStateChanged;
var
  CS: DWord;
  NewCardState:TCardState;
begin
  if FSCardReaderState.cbAtr<>0 then DecodeATR(@(FSCardReaderState.rgbATR)[0],FSCardReaderState.cbATR);
  CS:=FSCardReaderState.dwEventState;
  if (CS and SCARD_STATE_PRESENT <> 0) then begin
    if (CS and SCARD_STATE_EXCLUSIVE <> 0) then NewCardState:=csExclusive
    else if (CS and SCARD_STATE_INUSE <> 0) then NewCardState:=csShared
    else if (CS and SCARD_STATE_MUTE <> 0) then NewCardState:=csBadCard
    else NewCardState:=csAvailable;
  end
  else if (CS and SCARD_STATE_EMPTY) <> 0 then NewCardState:=csNoCard
  else if FSCardReaderState.cbAtr=0 then NewCardState:=csNoCard
  else NewCardState:=csUnknown;

  if NewCardState<>FCardState then begin
    FCardState:=NewCardState;
    if (FCardState=csNoCard) then FCardHandle:=INVALID_HANDLE_VALUE;
    if Assigned(FOnCardStateChanged) then FOnCardStateChanged(self);
  end;
end;

end.


