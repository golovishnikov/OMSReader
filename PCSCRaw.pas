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
                                                                                
{$OPTIMIZATION OFF}

unit PCSCRaw;

interface

uses
  Windows, SysUtils, Classes, PcscDef;

type
  TPCSCRaw = class(TObject)
  private
    FValid: boolean;
    FhWinSCard: THandle;

    _SCardBeginTransaction: function(hContext: DWord): DWord; stdcall;
    _SCardCancel: function(hContext: DWord): DWord; stdcall;
    _SCardConnectA: function(hContext: DWord; pReader: PChar; ShareMode: DWord; PreferredProtocols: DWord; var CardHandle, ActiveProtocol: DWord): DWord; stdcall;
    _SCardControl: function(hContext, IoCtl: DWord; pInBuffer: Pointer; SizeInBuffer: DWord; pOutBuffer: Pointer; SizeOutBuffer: DWord; var BytesRet: DWord): DWord; stdcall;
    _SCardDisconnect: function(hContext, Disposition: DWord): DWord; stdcall;
    _SCardDlgExtendedError: function: DWord; stdcall;
    _SCardEndTransaction: function(hContext,Disposition: DWord): DWord; stdcall;
    _SCardEstablishContext: function(Scope: DWord; pReserved1, pReserved2: Pointer; var hContext: DWord): DWord; stdcall;
    _SCardFreeMemory: function(hContext: DWord; pMem: Pointer): DWord; stdcall;
    _SCardGetAttrib: function(hCard, AttrId: DWord; pAttr: Pointer; var SizeAttr: DWord): DWord; stdcall;
    _SCardGetProviderIdA: function(hContext: DWord; pCard: PChar; var pGuidProviderId: TGUID): DWord; stdcall;
    _SCardGetStatusChangeA: function(hContext, Timeout: DWord; pReaderStates: PSCardReaderStateA; ReaderStatesCount: DWord): DWord; stdcall;
    _SCardListReadersA: function(hContext: DWord; pGroups, pReaders: PChar;var SizeReaders: DWord): DWord; stdcall;
    _SCardListCardsA: function(hContext: DWord; pAtr: Pointer; pGuidInterfaces: PGUID;GuidInterfacesCount: DWord; pCards: PChar;var SizeCards: DWord): DWord; stdcall;
    _SCardListInterfacesA: function(hContext: DWord; pCard: PChar;pGuidInterfaces: PGUID;var GuidInterfacesCount: DWord): DWord; stdcall;
    _SCardListReaderGroupsA: function(hContext: DWord; pGroups: PChar;var SizeGroups: DWord): DWord; stdcall;
    _SCardLocateCardsA: function(hContext: DWord; pCards: PChar;pReaderStates: PScardReaderStateA;ReaderStatesCount: DWord): DWord; stdcall;
    _SCardReconnect: function(hCard,ShareMode, PreferredProtocols, dwInitialization: DWord;var ActiveProtocol: DWord): DWord; stdcall;
    _SCardReleaseContext: function(hContext: DWord): DWord; stdcall;
    _SCardTransmit: function(hCard: DWord; PioSendPci: PSCardIoRequest;pSendBuffer: Pointer; SizeSendBuffer: DWord;PioRecvPci: PSCardIoRequest; pRecvBuffer: Pointer;var SizeRecvBuffer: DWord): DWord; stdcall;
    _SCardSetAttrib: function(hCard, AttrID: DWord; pAttr: Pointer;AttrLen: DWord): DWord; stdcall;
    _SCardStatusA: function (hCard: DWord; pReaderNames: PChar;var SiteReaderNames, State, Protocol: DWord;pAtr: Pointer; var AtrLen: DWord): DWord; stdcall;
  public
    property hWinSCard: THandle read FhWinSCard;

    constructor Create;
    destructor Destroy; override;

    function Initialize: DWord;
    function Shutdown: Boolean;
    function SCardBeginTransaction(hContext: DWord): DWord;
    function SCardCancel(hContext: DWord): DWord;
    function SCardConnect(hContext: DWord; pReader: PChar;ShareMode, PreferredProtocols: DWord;var CardHandle, ActiveProtocol: DWord): DWord;
    function SCardControl(hContext,IoCtl: DWord; pInBuffer: Pointer; SizeInBuffer: DWord;pOutBuffer: Pointer; SizeOutBuffer: DWord;var BytesRet: DWord): DWord;
    function SCardDisconnect(hContext, Disposition: DWord): DWord;
    procedure SCardDlgExtendedError;
    function SCardEndTransaction(hContext, Disposition: DWord): DWord;
    function SCardEstablishContext(Scope: DWord; pReserved1, pReserved2: Pointer;var hContext: DWord): DWord;
    function SCardFreeMemory(hContext: DWord; pMem: Pointer): DWord;
    function SCardGetAttrib(hCard, AttrId: DWord; pAttr: Pointer;var SizeAttr: DWord): DWord;
    function SCardGetProviderId(hContext: DWord; pCard: PChar;var GuidProviderId: TGUID): DWord;
    function SCardGetStatusChange(hContext, Timeout: DWord;pReaderStates: PSCardReaderStateA;ReadersStatesCount: DWord): DWord;
    function SCardListReaders(hContext: DWord; pGroups, pReaders: PChar;var SizeReaders: DWord): DWord;
    function SCardListCards(hContext: DWord; pAtr: Pointer; pGuidInterfaces: PGUID;GuidInterfacesCount: DWord; pCards: PChar;var SizeCards: DWord): DWord;
    function SCardListInterfaces(hContext: DWord; pCard: PChar;pGuidInterfaces: PGUID;var GuidInterfacesCount: DWord): DWord;
    function SCardListReaderGroups(hContext: DWord; pGroups: PChar;var SizeGroups: DWord): DWord;
    function SCardLocateCards(hContext: DWord; pCards: PChar;pReaderStates: PScardReaderStateA;ReaderStatesCount: DWord): DWord;
    function SCardReconnect(hCard,ShareMode, PreferredProtocols, dwInitialization: DWord;var ActiveProtocol: DWord): DWord;
    function SCardReleaseContext(hContext: DWord): DWord;
    function SCardTransmit(hCard: DWord; PioSendPci: PSCardIoRequest;SendBuffer: Pointer; SizeSendBuffer: DWord;PioRecvPci: PSCardIoRequest; RecvBuffer: Pointer;var SizeRecvBuffer: DWord): DWord;
    function SCardSetAttrib(hCard, AttrID: DWord; pAttr: Pointer;SizeAttr: DWord): DWord;
    function SCardStatus(hCard: DWord; pReaderNames: PChar;var SizeReaderNames, State, Protocol: DWord;pAtr: Pointer; var SizeAtr: DWord): DWord;

    // additional functions
    function ScErrToStr(ErrorCode: DWord): String;
    function ScErrToSymbol(ErrorCode: DWord): String;

    function SCardCTLCode(Code: DWord): DWord;
    function CTLCode(DeviceType, _Function, Method, Access: DWord): DWord;

    property Valid:boolean read FValid;
  end;

const
  TPCSC_INIT_OK                 = 0;
  TPCSC_INIT_DLL_NOT_FOUND      = 1;
  TPCSC_INIT_SUPPORT_INCOMPLETE = 2;

function MultiStrToStringList(pBuffer: Pointer; SizeStr: LongInt; StrLst: TStringList): LongInt;

implementation

// MultiString:  str1#0str2#0str3#0.....strN#0#0
function MultiStrToStringList(pBuffer: Pointer; SizeStr: LongInt; StrLst: TStringList): LongInt;
var
  I,L: LongInt;
  MyStr:ShortString;
  pStr: PChar;
begin
  pStr:=pBuffer;
  Result:=-1;
  if StrLst=nil then exit;
  StrLst.Clear;
  L:=0;
  if ((pStr=nil) or (SizeStr=0)) then Result:=0
  else if (SizeStr=1) then begin
    if (pStr^=#0) then Result:=0
    else Result:=-1
  end
  else if (((pStr+SizeStr-1)^<>#0) or ((pStr+SizeStr-2)^<>#0)) then Result:=-1
  else begin
    for I:=0 to SizeStr-2 do begin
      if (pStr+I)^=#0 then begin
        SetLength(MyStr, L);
        Move((pStr+I-L)^,MyStr[1],L);
        if MyStr<>'' then StrLst.Add(MyStr);
        L:=0;
      end
      else Inc(L);
      Result:=0;
    end;
  end;
end;

constructor TPCSCRaw.Create;
begin
  inherited Create;
  FhWinSCard:=0;
  FValid:=false;
end;

destructor TPCSCRaw.Destroy;
begin
  if FValid then Shutdown;
  inherited Destroy;
end;

function TPCSCRaw.SCardBeginTransaction(hContext: DWord): DWord;
begin
  if not FValid then begin Result:=DLL_ERROR; exit end;
  Result:=_SCardBeginTransaction(hContext);
end;

function TPCSCRaw.SCardCancel(hContext: DWord): DWord;
begin
  if not FValid then begin Result:=DLL_ERROR; exit end;
  Result:=_SCardCancel(hContext);
end;

function TPCSCRaw.SCardConnect(hContext: DWord; pReader: PChar; ShareMode: DWord;PreferredProtocols: DWord;var CardHandle, ActiveProtocol: DWord): DWord;
begin
  if not FValid then begin Result:=DLL_ERROR; exit end;
  Result:=_SCardConnectA(hContext, pReader, ShareMode, PreferredProtocols,CardHandle, ActiveProtocol);
end;

function TPCSCRaw.SCardControl(hContext, IoCtl: DWord; pInBuffer: Pointer;SizeInBuffer: DWord; pOutBuffer: Pointer;SizeOutBuffer: DWord; var BytesRet: DWord): DWord;
begin
  if not FValid then begin Result:=DLL_ERROR; exit end;
  Result:=_SCardControl(hContext, IoCtl, pInBuffer, SizeInBuffer, pOutBuffer,SizeOutBuffer, BytesRet);
end;

function TPCSCRaw.SCardDisconnect(hContext, Disposition: DWord): DWord;
begin
  if not FValid then begin Result:=DLL_ERROR; exit end;
  Result:=_SCardDisconnect(hContext, Disposition);
end;

procedure TPCSCRaw.SCardDlgExtendedError;
begin
  if FValid then _SCardDlgExtendedError;
end;

function TPCSCRaw.SCardEndTransaction(hContext,Disposition: DWord): DWord;
begin
  if not FValid then begin Result:=DLL_ERROR; exit end;
  Result:=_SCardEndTransaction(hContext,Disposition);
end;

function TPCSCRaw.SCardEstablishContext(Scope: DWord; pReserved1, pReserved2: Pointer;var hContext: DWord): DWord;
begin
  if not FValid then begin Result:=DLL_ERROR; exit end;
  Result:=_SCardEstablishContext(Scope, pReserved1, pReserved2, hContext);
end;

function TPCSCRaw.SCardFreeMemory(hContext: DWord; pMem: Pointer): DWord;
begin
  if not FValid then begin Result:=DLL_ERROR; exit end;
  Result:=_SCardFreeMemory(hContext, pMem);
end;

function TPCSCRaw.SCardGetAttrib(hCard, AttrId: DWord; pAttr: Pointer;var SizeAttr: DWord): DWord;
begin
  if not FValid then begin Result:=DLL_ERROR; exit end;
  Result:=_SCardGetAttrib(hCard, AttrId, pAttr, SizeAttr);
end;

function TPCSCRaw.SCardGetProviderId(hContext: DWord; pCard: PChar;var GuidProviderId: TGUID): DWord;
begin
  if not FValid then begin Result:=DLL_ERROR; exit end;
  Result:=_SCardGetProviderIdA(hContext, pCard, GuidProviderId);
end;

function TPCSCRaw.SCardGetStatusChange(hContext,Timeout: DWord;pReaderStates: PSCardReaderStateA;ReadersStatesCount: DWord): DWord;
begin
  if not FValid then begin Result:=DLL_ERROR; exit end;
  Result:=_SCardGetStatusChangeA(hContext,Timeout, pReaderStates, ReadersStatesCount);
end;

function TPCSCRaw.SCardListReaders(hContext: DWord; pGroups, pReaders: PChar;var SizeReaders: DWord): DWord;
begin
  if not FValid then begin Result:=DLL_ERROR; exit end;
  Result:=_SCardListReadersA(hContext, pGroups, pReaders, SizeReaders);
end;

function TPCSCRaw.SCardListCards(hContext: DWord; pAtr: Pointer; pGuidInterfaces: PGUID;GuidInterfacesCount: DWord; pCards: PChar;var SizeCards: DWord): DWord;
begin
  if not FValid then begin Result:=DLL_ERROR; exit end;
  Result:=_SCardListCardsA(hContext, pAtr, pGuidInterfaces, GuidInterfacesCount,pCards, SizeCards);
end;

function TPCSCRaw.SCardListInterfaces(hContext: DWord; pCard: PChar;pGuidInterfaces: PGUID;var GuidInterfacesCount: DWord): DWord;
begin
  if not FValid then begin Result:=DLL_ERROR; exit end;
  Result:=_SCardListInterfacesA(hContext, pCard, pGuidInterfaces, GuidInterfacesCount);
end;

function TPCSCRaw.SCardListReaderGroups(hContext: DWord; pGroups: PChar;var SizeGroups: DWord): DWord;
begin
  if not FValid then begin Result:=DLL_ERROR; exit end;
  Result:=_SCardListReaderGroupsA(hContext, pGroups, SizeGroups);
end;

function TPCSCRaw.SCardLocateCards(hContext: DWord; pCards: PChar;pReaderStates: PScardReaderStateA;ReaderStatesCount: DWord): DWord;
begin
  if not FValid then begin Result:=DLL_ERROR; exit end;
  Result:=_SCardLocateCardsA(hContext, pCards, pReaderStates, ReaderStatesCount);
end;

function TPCSCRaw.SCardReconnect(hCard,ShareMode, PreferredProtocols, dwInitialization: DWord;var ActiveProtocol: DWord): DWord;
begin
  if not FValid then begin Result:=DLL_ERROR; exit end;
  Result:=_SCardReconnect(hCard,ShareMode, PreferredProtocols, dwInitialization,ActiveProtocol);
end;

function TPCSCRaw.SCardReleaseContext(hContext: DWord): DWord;
begin
  if not FValid then begin Result:=DLL_ERROR; exit end;
  Result:=_SCardReleaseContext(hContext);
end;

function TPCSCRaw.SCardTransmit(hCard: DWord; PioSendPci: PSCardIoRequest;SendBuffer: Pointer; SizeSendBuffer: DWord;PioRecvPci: PSCardIoRequest; RecvBuffer: Pointer;var SizeRecvBuffer: DWord): DWord;
begin
  if not FValid then begin Result:=DLL_ERROR; exit end;
  Result:=_SCardTransmit(hCard, PioSendPci, SendBuffer, SizeSendBuffer,PioRecvPci, RecvBuffer, SizeRecvBuffer);
end;

function TPCSCRaw.SCardSetAttrib(hCard, AttrID: DWord; pAttr: Pointer;SizeAttr: DWord): DWord;
begin
  if not FValid then begin Result:=DLL_ERROR; exit end;
  Result:=_SCardSetAttrib(hCard, AttrID, pAttr, SizeAttr);
end;

function TPCSCRaw.SCardStatus(hCard: DWord; pReaderNames: PChar;var SizeReaderNames, State, Protocol: DWord;pAtr: Pointer; var SizeAtr: DWord): DWord;
begin
  if not FValid then begin Result:=DLL_ERROR; exit end;
  Result:=_SCardStatusA(hCard, pReaderNames, SizeReaderNames, State, Protocol,pAtr, SizeAtr);
end;

{--------------------------- additional functions -----------------------------}

//#define CTL_CODE( DeviceType, Function, Method, Access )     (((DeviceType) << 16) | ((Access) << 14) | ((Function) << 2) | (Method))
function TPCSCRaw.CTLCode(DeviceType, _Function, Method, Access: DWord): DWord;
begin
   Result:=(DeviceType shl 16) or (Access shl 14) or (_Function shl 2) or Method;
end;

//#define SCARD_CTL_CODE(code)        CTL_CODE(FILE_DEVICE_SMARTCARD, (code), METHOD_BUFFERED, FILE_ANY_ACCESS)
function TPCSCRaw.SCardCTLCode(Code: DWord): DWord;
begin
   Result:=CTLCode(FILE_DEVICE_SMARTCARD, Code, METHOD_BUFFERED, FILE_ANY_ACCESS);
end;

function TPCSCRaw.SCErrToStr(ErrorCode: DWord): string;
var
  Flags: DWord;
  buff: array[0..255] of char;
begin
    Flags:=FORMAT_MESSAGE_FROM_HMODULE or FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_IGNORE_INSERTS;
    {Len:=}FormatMessage(Flags,Pointer(fhWinSCard), ErrorCode, 0, @buff[0], 250, nil);
    result:=StrPas(buff);
    if (Result='') then begin
      (* this should not happen, but we've seen it.... *)
      Result:=IntToHex(GetLastError(), 8);
      Exit;
    end;
    if Copy(Result,Length(Result)-1,2)=#13#10 then SetLength(Result, Length(Result)-2);
end;

function TPCSCRaw.ScErrToSymbol(ErrorCode: DWord):String;
begin
  Result:='';
  case ErrorCode of
    SCARD_F_COMM_ERROR             : Result:='F_COMM_ERROR';
    SCARD_F_INTERNAL_ERROR         : Result:='F_INTERNAL_ERROR';
    SCARD_F_UNKNOWN_ERROR          : Result:='F_UNKNOWN_ERROR';
    SCARD_F_WAITED_TOO_LONG        : Result:='F_WAITED_TOO_LONG';
    SCARD_E_BAD_SEEK               : Result:='E_BAD_SEEK';
    SCARD_E_CANCELLED              : Result:='E_CANCELLED';
    SCARD_E_CANT_DISPOSE           : Result:='E_CANT_DISPOSE';
    SCARD_E_CARD_UNSUPPORTED       : Result:='E_CARD_UNSUPPORTED';
    SCARD_E_CERTIFICATE_UNAVAILABLE: Result:='E_CERTIFICATE_UNAVAILABLE';
    SCARD_E_DIR_NOT_FOUND          : Result:='E_DIR_NOT_FOUND';
    SCARD_E_DUPLICATE_READER       : Result:='E_DUPLICATE_READER';
    SCARD_E_FILE_NOT_FOUND         : Result:='E_FILE_NOT_FOUND';
    SCARD_E_ICC_CREATEORDER        : Result:='E_ICC_CREATEORDER';
    SCARD_E_ICC_INSTALLATION       : Result:='E_ICC_INSTALLATION';
    SCARD_E_INSUFFICIENT_BUFFER    : Result:='E_INSUFFICIENT_BUFFER';
    SCARD_E_INVALID_ATR            : Result:='E_INVALID_ATR';
    SCARD_E_INVALID_CHV            : Result:='E_INVALID_CHV';
    SCARD_E_INVALID_HANDLE         : Result:='E_INVALID_HANDLE';
    SCARD_E_INVALID_PARAMETER      : Result:='E_INVALID_PARAMETER';
    SCARD_E_INVALID_TARGET         : Result:='E_INVALID_TARGET';
    SCARD_E_INVALID_VALUE          : Result:='E_INVALID_VALUE';
    SCARD_E_NO_ACCESS              : Result:='E_NO_ACCESS';
    SCARD_E_NO_DIR                 : Result:='E_NO_DIR';
    SCARD_E_NO_FILE                : Result:='E_NO_FILE';
    SCARD_E_NO_MEMORY              : Result:='E_NO_MEMORY';
    SCARD_E_NO_READERS_AVAILABLE   : Result:='E_NO_READERS_AVAILABLE';
    SCARD_E_NO_SERVICE             : Result:='E_NO_SERVICE';
    SCARD_E_NO_SMARTCARD           : Result:='E_NO_SMARTCARD';
    SCARD_E_NO_SUCH_CERTIFICATE    : Result:='E_NO_SUCH_CERTIFICATE';
    SCARD_E_NOT_READY              : Result:='E_NOT_READY ';
    SCARD_E_NOT_TRANSACTED         : Result:='E_NOT_TRANSACTED';
    SCARD_E_PCI_TOO_SMALL          : Result:='E_PCI_TOO_SMALL';
    SCARD_E_PROTO_MISMATCH         : Result:='E_PROTO_MISMATCH';
    SCARD_E_READER_UNAVAILABLE     : Result:='E_READER_UNAVAILABLE';
    SCARD_E_READER_UNSUPPORTED     : Result:='E_READER_UNSUPPORTED';
    SCARD_E_SERVICE_STOPPED        : Result:='E_SERVICE_STOPPED';
    SCARD_E_SHARING_VIOLATION      : Result:='E_SHARING_VIOLATION';
    SCARD_E_SYSTEM_CANCELLED       : Result:='E_SYSTEM_CANCELLED';
    SCARD_E_TIMEOUT                : Result:='E_TIMEOUT';
    SCARD_E_UNEXPECTED             : Result:='E_UNEXPECTED';
    SCARD_E_UNKNOWN_CARD           : Result:='E_UNKNOWN_CARD';
    SCARD_E_UNKNOWN_READER         : Result:='E_UNKNOWN_READER';
    SCARD_E_UNKNOWN_RES_MNG        : Result:='E_UNKNOWN_RES_MNG';
    SCARD_E_UNSUPPORTED_FEATURE    : Result:='E_UNSUPPORTED_FEATURE';
    SCARD_E_WRITE_TOO_MANY         : Result:='E_WRITE_TOO_MANY';
    SCARD_P_SHUTDOWN               : Result:='P_SHUTDOWN';
    SCARD_S_SUCCESS                : Result:='S_SUCCESS';
    SCARD_W_CANCELLED_BY_USER      : Result:='W_CANCELLED_BY_USER';
    SCARD_W_CHV_BLOCKED            : Result:='W_CHV_BLOCKED';
    SCARD_W_EOF                    : Result:='W_EOF';
    SCARD_W_REMOVED_CARD           : Result:='W_REMOVED_CARD';
    SCARD_W_RESET_CARD             : Result:='W_RESET_CARD';
    SCARD_W_SECURITY_VIOLATION     : Result:='W_SECURITY_VIOLATION';
    SCARD_W_UNPOWERED_CARD         : Result:='W_UNPOWERED_CARD';
    SCARD_W_UNRESPONSIVE_CARD      : Result:='W_UNRESPONSIVE_CARD';
    SCARD_W_UNSUPPORTED_CARD       : Result:='W_UNSUPPORTED_CARD';
    SCARD_W_WRONG_CHV              : Result:='W_WRONG_CHV';
    else Result:='??';
  end;
  Result:='SCARD_'+Result;
end;

function TPCSCRaw.Shutdown: Boolean;
begin
  if FValid then begin
    Result:=True;
    FValid:=False;
    FreeLibrary(fhWinSCard);
    FhWinSCard:=0;
  end
  else Result:=False;
end;

function TPCSCRaw.Initialize: DWord;
var
  tmpDLLH: THandle;
  tmpValid: boolean;
begin
  // Try to load the DLLs
  tmpDLLH:=LoadLibrary('WINSCARD.DLL');
  if (tmpDLLH=0) then begin
    result:=TPCSC_INIT_DLL_NOT_FOUND;
    exit;
  end;

  // Assign the functions
  _SCardBeginTransaction:=nil;
  _SCardCancel:=nil;
  _SCardConnectA:=nil;
  _SCardControl:=nil;
  _SCardDisconnect:=nil;
  _SCardEndTransaction:=nil;
  _SCardEstablishContext:=nil;
  _SCardFreeMemory:=nil;
  _SCardGetAttrib:=nil;
  _SCardGetProviderIdA:=nil;
  _SCardGetStatusChangeA:=nil;
  _SCardListReadersA:=nil;
  _SCardListCardsA:=nil;
  _SCardListInterfacesA:=nil;
  _SCardListReaderGroupsA:=nil;
  _SCardLocateCardsA:=nil;
  _SCardReconnect:=nil;
  _SCardReleaseContext:=nil;
  _SCardTransmit:=nil;
  _SCardSetAttrib:=nil;
  _SCardStatusA:=nil;

  @_SCardBeginTransaction:=GetProcAddress(tmpDLLH,  'SCardBeginTransaction');
  @_SCardCancel:=GetProcAddress(tmpDLLH, 'SCardCancel');
  @_SCardConnectA:=GetProcAddress(tmpDLLH, 'SCardConnectA');
  @_SCardControl:=GetProcAddress(tmpDLLH, 'SCardControl');
  @_SCardDisconnect:=GetProcAddress(tmpDLLH, 'SCardDisconnect');
  @_SCardEndTransaction:=GetProcAddress(tmpDLLH, 'SCardEndTransaction');
  @_SCardEstablishContext:=GetProcAddress(tmpDLLH, 'SCardEstablishContext');
  @_SCardFreeMemory:=GetProcAddress(tmpDLLH, 'SCardFreeMemory');
  @_SCardGetAttrib:=GetProcAddress(tmpDLLH, 'SCardGetAttrib');
  @_SCardGetProviderIdA:=GetProcAddress(tmpDLLH, 'SCardGetProviderIdA');
  @_SCardGetStatusChangeA:=GetProcAddress(tmpDLLH, 'SCardGetStatusChangeA');
  @_SCardListReadersA:=GetProcAddress(tmpDLLH, 'SCardListReadersA');
  @_SCardListCardsA:=GetProcAddress(tmpDLLH, 'SCardListCardsA');
  @_SCardListInterfacesA:=GetProcAddress(tmpDLLH, 'SCardListInterfacesA');
  @_SCardListReaderGroupsA:=GetProcAddress(tmpDLLH, 'SCardListReaderGroupsA');
  @_SCardLocateCardsA:=GetProcAddress(tmpDLLH, 'SCardLocateCardsA');
  @_SCardReconnect:=GetProcAddress(tmpDLLH, 'SCardReconnect');
  @_SCardReleaseContext:=GetProcAddress(tmpDLLH, 'SCardReleaseContext');
  @_SCardTransmit:=GetProcAddress(tmpDLLH, 'SCardTransmit');
  @_SCardSetAttrib:=GetProcAddress(tmpDLLH, 'SCardSetAttrib');
  @_SCardStatusA:=GetProcAddress(tmpDLLH, 'SCardStatusA');

  tmpValid:=
    Assigned(_SCardBeginTransaction) and
    Assigned(_SCardCancel) and
    Assigned(_SCardConnectA) and
    Assigned(_SCardControl) and
    Assigned(_SCardDisconnect) and
    Assigned(_SCardEndTransaction) and
    Assigned(_SCardEstablishContext) and
    Assigned(_SCardFreeMemory) and
    Assigned(_SCardGetAttrib) and
    Assigned(_SCardGetProviderIdA) and
    Assigned(_SCardGetStatusChangeA) and
    Assigned(_SCardListReadersA) and
    Assigned(_SCardListCardsA) and
    Assigned(_SCardListInterfacesA) and
    Assigned(_SCardListReaderGroupsA) and
    Assigned(_SCardLocateCardsA) and
    Assigned(_SCardReconnect) and
    Assigned(_SCardReleaseContext) and
    Assigned(_SCardTransmit) and
    Assigned(_SCardSetAttrib) and
    Assigned(_SCardStatusA);

  // Assign the Object's properties
  if tmpValid then begin
     FValid:=True;
     Result:=TPCSC_INIT_OK;
     FhWinSCard:=tmpDLLH;
  end
  else Result:=TPCSC_INIT_SUPPORT_INCOMPLETE;
end;

end.

