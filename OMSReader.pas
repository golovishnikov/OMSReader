unit OMSReader;

interface

uses
  Windows, Classes, Types, SysUtils, Controls, Contnrs,
  PCSCRaw, PcscDef, Reader,
  DateUtils;

type
  // Данные владельца карты
  TOwnerInfo = record
    MedicalInsuranceNumber: String;   // Номер полиса
    RegistrationDate: TDate; // Дата создания карточки?
    MedicalInsuranceDateEnd: TDate;   // Действует до, может 0, значит не ограничен
    Policy: String;                   // Номер карточки?
    LName: String;                    // Фамилия
    FName: String;                    // Имя
    MName: String;                    // Отчество
    SexName: String;                  // Пол
    Byrthday: TDate;                  // Дата рождения
    BirthPlace: String;               // Место рождения
    CountryCode: String;              // Код страны
    Country: String;                  // Название страны
    InsuranceCertificate: String;     // СНИЛС
    ImgFormat: string;                // Формат изображения
    Img: AnsiString;                  // Данные фоторгафии, соответствующие ImgFormat
    OGRN: String;                     // ОГРН для СМО
    OKATO: String;                    // ОКАТО для СМО
    DateBegin: TDate;                 // Дата начала страхования в СМО
    DateEnd: TDate;                   // Дата окончания страхования в СМО
  end;

  // Состояние объекта, следящего за ридером
  TOMSReaderState = (
    rsSuccess, // В ридер вставлена карта полиса и она прочитана
    rsUnknown  // Карта не вставлена или ее не удалось прочесть
  );

  // Данные полученные от ридера
  TTransmitResponse = record
    SW1: byte;
    SW2: byte;
    Data: AnsiString;
  end;
  PTransmitResponse = ^TTransmitResponse;
  // Обертка вокруг объекта работы с картридером
  // Расширяет свойства и перекрывает процедуру обработки смены состояния,
  // что очень важно
  TReaderObjectExt = class(TReaderObject)
  private
    FLockRead: Integer;
    FOldCardState: TCardState;
    FOldCardStateTemp: TCardState;
    FOnCardStateChanged: TNotifyEvent;
    procedure DoOnCardStateChanged(Sender: TObject);
    procedure LockRead;
    procedure UnLockRead;
    function IsLockRead: Boolean;
  public
    constructor Create(AReaderName: String; PCSCRaw: TPCSCRaw);
    destructor Destroy; override;

    property OldCardState: TCardState read FOldCardState;
    property OnCardStateChanged: TNotifyEvent read FOnCardStateChanged write FOnCardStateChanged;
  end;

  // Тип события для логгирования
  TLogType = (
    ltInit,           // Инициализация драйвера
    ltReaderListChange,// Изменился список ридеров
    ltCardStateChange,// Состояние карточки изменилось
    ltConnect,        // Подключение к карточке
    ltDisconnect,     // Отключение
    ltSend,           // Передача команды
    ltRecive,         // Получение данных
    ltMessage         // Простое сообщение в лог
  );


  // Прототип функции для записи тестовых сообщений
  // Msg - это в том числе и отправляемые/получаемые данные
  TOnLog = procedure(LogType: TLogType; FunctionResult: DWORD; Msg: String) of object;

  TReaderList = class(TObjectList)
  private
    function GetReader(Index: Integer): TReaderObjectExt;
    procedure SetReader(Index: Integer; const Value: TReaderObjectExt);
  public
    property Readers[Index: Integer]: TReaderObjectExt read GetReader write SetReader;
    function IndexOfName(ReaderName: String): Integer;
  end;

  TOMSReader = class(TObject)
  private
    FState: TOMSReaderState;
    FOwnerInfo: TOwnerInfo;
    FReaderName: String;
    FPCSCRaw: TPCSCRaw;
    FPCSCDeviceContext: DWORD;
    FReaderList: TReaderList;
    FReaderListThread: TReaderListThread;
    FOnLog: TOnLog;
    FOnSatetChange: TNotifyEvent;
    procedure InitPCSC;
    procedure UpdatePCSCReaderList;
    procedure GetPCSCReaderList(ReaderList: TStringList);
    procedure CardStateChanged(Sender: TObject);
    procedure ReaderListChanged;
    procedure AddLog(LogType: TLogType; FunctionResult: DWORD; Msg: String);
    procedure ReadData(Reader: TReaderObjectExt);
    function Connect(Reader: TReaderObjectExt): Boolean;
    procedure Disconnect(Reader: TReaderObjectExt);
    procedure Transmit(Reader: TReaderObjectExt; Command: Pointer; CLen: Integer; var Response: TTransmitResponse);
    procedure ParseResponse(Buffer: Pointer; BufferSize: Integer; var Response: TTransmitResponse);
    procedure SetOwnerInfo(AReaderName: String; Buffer: Pointer; BufferSize: Integer);
  public
    constructor Create(AOnLog: TOnLog = nil);
    destructor Destroy; override;
    function ReadOwnerInfo(var OwnerInfo: TOwnerInfo): Boolean;

    property State: TOMSReaderState read FState;
    property OnLog: TOnLog read FOnLog write FOnLog;
    property ReaderName: String read FReaderName;
    property OnSatetChange: TNotifyEvent read FOnSatetChange write FOnSatetChange;
  end;

function StateName(State: TCardState): String;

implementation

type
  // Список полей владельца карты ОМС
  TOwnerInfoFields = (
    oiMedicalInsuranceNumber,     // Номер полиса
    oiRegistrationDate,  // Дата создания карточки?
    oiMedicalInsuranceDateEnd,    // Действует до, может 0, значит не ограничен
    oiPolicy,                     // Номер карточки?
    oiLName,                      // Фамилия
    oiFName,                      // Имя
    oiMName,                      // Отчество
    oiSexName,                    // Пол
    oiByrthday,                   // Дата рождения
    oiBirthPlace,                 // Место рождения
    oiCountryCode,                // Код страны
    oiCountry,                    // Название страны
    oiInsuranceCertificate,       // СНИЛС
    oiImgFormat,                  // Формат изображения
    oiImg,                        // Данные фоторгафии, соответствующие ImgFormat
    oiOGRN,                       // ОГРН для СМО
    oiOKATO,                      // ОКАТО для СМО
    oiDateBegin,                  // Дата начала страхования в СМО
    oiDateEnd                     // Дата окончания страхования в СМО
  );

var
  // --- выбор неизменяемых данных ---
  SELECT_DIR_CONST: array[0..11] of Byte = ($00, $a4, $04, $0c, $07, $46, $4f, $4d, $53, $5f, $49, $44);
  SELECT_FILE_CONST: array[0..6] of Byte = ($00, $a4, $02, $0c, $02, $02, $01);
  READ_FILE_CONST: array[0..4] of Byte = ($00, $b0, $00, $00, $00);
  // --- выбор изменяемых данных ---
  SELECT_DIR_CHANGE: array[0..12] of Byte =  ($00, $a4, $04, $0c, $07, $46, $4f, $4d, $53, $5f, $49, $4e, $53);
  READ_DIR_CHANGE: array[0..4] of Byte = ($00, $ca, $01, $b0, $02); // данная команда позволяет определить файл с актуальной информацией
  SELECT_FILE_CHANGE: array[0..4] of Byte = ($00, $a4, $02, $0c, $02); // имя нужного файла передается в двух байтах, которые добавятся в конец списка уже в программе.
  READ_FILE_CHANGE: array[0..4] of Byte = ($00, $b0, $00, $00, $00);

  OwnerInfoFieldsAddress: array[oiMedicalInsuranceNumber..oiDateEnd, 0..1] of Byte = (
    ($5f, $26), // Номер полиса
    ($5f, $2a), // Дата выдачи?
    ($5f, $28), // Действует до, может 0, значит не ограничен
    ($5f, $26), // Номер карточки?
    ($5f, $21), // Фамилия
    ($5f, $22), // Имя
    ($5f, $23), // Отчество
    ($5f, $25), // Пол
    ($5f, $24), // Дата рождения
    ($5f, $29), // Место рождения
    ($5f, $31), // Код страны
    ($5f, $32), // Название страны
    ($5f, $27), // СНИЛС
    ($5f, $41), // Формат изображения
    ($5f, $42), // Данные фоторгафии, соответствующие ImgFormat
    ($5f, $51), // ОГРН для СМО
    ($5f, $52), // ОКАТО для СМО
    ($5f, $53), // Дата начала страхования в СМО
    ($5f, $54)  // Дата окончания страхования в СМО
  );

function BufToString(Buff: Pointer; Len: Integer): String;
var
  I: Integer;
  P: PByte;
begin
  Result := '';
  P := Buff;
  for I := 0 to Len - 1 do
  begin
    Result := Result + IntToHex(P^, 2) + ' ';
    Inc(P);
  end;
end;

function CommandToBuffer(const A: array of Byte): TByteDynArray;
begin
  SetLength(Result, Length(A));
  Move(A[Low(A)], Result[0], Length(A));
end;


{ TOMSReader }

procedure TOMSReader.AddLog(LogType: TLogType; FunctionResult: DWORD; Msg: String);
begin
  if Assigned(FOnLog) then
    FOnLog(LogType, FunctionResult, Msg);
end;

function StateName(State: TCardState): String;
begin
  case State of
    csExclusive : Result := 'exclusive';
    csShared    : Result := 'shared';
    csAvailable : Result := 'available';
    csBadCard   : Result := 'bad card';
    csNoCard    : Result := 'no card';
    else          Result := 'unknown';
  end;
end;

procedure TOMSReader.CardStateChanged(Sender: TObject);
var
  ReaderName:string;
begin
  ReaderName := (Sender as TReaderObjectExt).ReaderName;
  AddLog(ltCardStateChange, Ord((Sender as TReaderObjectExt).CardState), 'Reader name: ' + ReaderName);
  ReadData(Sender as TReaderObjectExt);
end;

function TOMSReader.Connect(Reader: TReaderObjectExt): Boolean;
var
  PCSCResult: DWORD;
begin
  PCSCResult := Reader.SCConnect(FPCSCRaw, FPCSCDeviceContext, SCARD_SHARE_SHARED);
  AddLog(ltConnect, PCSCResult, 'SCardConnect');
  Result := PCSCResult = SCARD_S_SUCCESS;
end;

constructor TOMSReader.Create(AOnLog: TOnLog = nil);
begin
  inherited Create;
  FOnSatetChange := nil;
  FReaderName := '';
  FOnLog := AOnLog;
  FState := rsUnknown;
  FPCSCDeviceContext := 0;
  FPCSCRaw := TPCSCRaw.Create;
  FReaderList := TReaderList.Create;
  InitPCSC;
  ReaderListChanged;
  FReaderListThread:=TReaderListThread.Create(FPCSCRaw);
  FReaderListThread.OnReaderListChanged:=ReaderListChanged;
  FReaderListThread.Resume;
end;

destructor TOMSReader.Destroy;
begin
  FReaderListThread.Terminate;
  FReaderList.Free;

  if FPCSCDeviceContext<>0 then begin
    FPCSCRaw.SCardCancel(FPCSCDeviceContext);
    FPCSCRaw.SCardReleaseContext(FPCSCDeviceContext);
  end;
  FReaderListThread.Free;
  FPCSCRaw.Shutdown;
  FPCSCRaw.Free;
  inherited;
end;


procedure TOMSReader.Disconnect(Reader: TReaderObjectExt);
var
  PCSCResult:DWORD;
begin
  PCSCResult := Reader.SCDisconnect(FPCSCRaw, FPCSCDeviceContext);
  AddLog(ltConnect, PCSCResult,  'SCardDisconnect');
end;

procedure TOMSReader.GetPCSCReaderList(ReaderList: TStringList);
var
  pReaders: PChar;
  PCSCResult: DWORD;
  SizeReaders: DWORD;
begin
  ReaderList.Clear;

  PCSCResult := FPCSCRaw.SCardListReaders(FPCSCDeviceContext, nil, nil, SizeReaders);
  if PCSCResult = SCARD_S_SUCCESS then
  begin
    GetMem(pReaders, SizeReaders);
    try
      PCSCResult := FPCSCRaw.SCardListReaders(FPCSCDeviceContext, nil, pReaders, SizeReaders);
      if PCSCResult = SCARD_S_SUCCESS then
      begin
        MultiStrToStringList(pReaders,SizeReaders,ReaderList);
        AddLog(ltReaderListChange, PCSCResult, 'SCardListReaders suceeded.');
      end;
    finally
      if pReaders<>nil then FreeMem(pReaders);
    end;
  end
  else
  begin
    AddLog(ltReaderListChange, PCSCResult, 'SCardListReaders failed.');
  end;
end;

procedure TOMSReader.InitPCSC;
var
  PCSCResult: DWORD;
begin
  PCSCResult := FPCSCRaw.Initialize;
  if PCSCResult <> TPCSC_INIT_OK then
    AddLog(ltInit, PCSCResult, 'Cannot access Winscard.dll.')
  else
    AddLog(ltInit, PCSCResult, 'Winscard.dll successfully loaded.');

  // establishing PC/SC context
  PCSCResult := FPCSCRaw.SCardEstablishContext(SCARD_SCOPE_SYSTEM, nil, nil, FPCSCDeviceContext);
  AddLog(ltInit, PCSCResult, 'SCardEstablishContext.');
end;

procedure TOMSReader.ParseResponse(Buffer: Pointer; BufferSize: Integer;
  var Response: TTransmitResponse);
var
  P: PByte;
  I: Integer;
begin
  if BufferSize < 2 then exit;
  SetLength(Response.Data, BufferSize - 2);
  Move(Buffer^, Response.Data[1], BufferSize - 2);
  P := Buffer;
  for I := 1 to BufferSize - 2 do
    Inc(P);
  Response.SW1 := P^;
  Inc(P);
  Response.SW2 := P^;
end;

procedure TOMSReader.ReadData(Reader: TReaderObjectExt);
var
  I: Integer;
  SelDirConst: TTransmitResponse;
  SelFileConst: TTransmitResponse;
  ReadFileConst: TTransmitResponse;
  SelDirChange: TTransmitResponse;
  ReadDirChange: TTransmitResponse;
  SelFileChange: TTransmitResponse;
  ReadFileChange: TTransmitResponse;
  CommandBuff: array of byte;
  Data: AnsiString;
begin
  // Данные нужно читать только тогда, когда становится доступна карта,
  // а предудущее состояние было - карта не доступна
  if (Reader.CardState in [csUnknown, csBadCard, csNoCard]) and (Reader.OldCardState in [csExclusive, csShared, csAvailable]) then
  begin
    SetOwnerInfo(Reader.ReaderName, nil, 0);
    Exit;
  end;
  if not ((Reader.CardState = csAvailable) and (Reader.OldCardState in [csUnknown, csBadCard, csNoCard])) then Exit;
  if Reader.IsLockRead then Exit;

  Reader.LockRead;
  try
    if Connect(Reader) then
    begin
      Transmit(Reader, CommandToBuffer(SELECT_DIR_CONST), Length(SELECT_DIR_CONST), SelDirConst);
      if (SelDirConst.SW1 = 144) and (SelDirConst.SW2 = 0) then
      begin
        Transmit(Reader, CommandToBuffer(SELECT_FILE_CONST), Length(SELECT_FILE_CONST), SelFileConst);
        Transmit(Reader, CommandToBuffer(READ_FILE_CONST), Length(READ_FILE_CONST), ReadFileConst); // Неизменяемые данные
        Transmit(Reader, CommandToBuffer(SELECT_DIR_CHANGE), Length(SELECT_DIR_CHANGE), SelDirChange);
        Transmit(Reader, CommandToBuffer(READ_DIR_CHANGE), Length(READ_DIR_CHANGE), ReadDirChange);
        SetLength(CommandBuff, Length(SELECT_FILE_CHANGE) + 2);
        Move(SELECT_FILE_CHANGE[0], CommandBuff[0], Length(SELECT_FILE_CHANGE));
        CommandBuff[Length(CommandBuff)-2] := Ord(ReadDirChange.Data[1]);
        CommandBuff[Length(CommandBuff)-1] := Ord(ReadDirChange.Data[2]);
        Transmit(Reader, CommandBuff, Length(CommandBuff), SelFileChange);
        Transmit(Reader, CommandToBuffer(READ_FILE_CHANGE), Length(READ_FILE_CHANGE), ReadFileChange);
        Data := ReadFileConst.Data + ReadFileChange.Data;
        SetOwnerInfo(Reader.ReaderName, @Data[1], Length(Data));
      end
      else if SelDirConst.SW1 = $6a then
      begin
        AddLog(ltSend, SelDirConst.SW1, 'Unknow card ' + Reader.ReaderName);
        SetOwnerInfo(Reader.ReaderName, nil, 0);
      end
      else
      begin
        AddLog(ltSend, SelDirConst.SW1, 'Unknow card ' + Reader.ReaderName);
        SetOwnerInfo(Reader.ReaderName, nil, 0);
      end;
      Disconnect(Reader);
    end
    else
    begin
      SetOwnerInfo(Reader.ReaderName, nil, 0);
    end;
  finally
    Reader.UnLockRead;
  end;
end;

procedure TOMSReader.ReaderListChanged;
begin
  UpdatePCSCReaderList;
end;

function TOMSReader.ReadOwnerInfo(
  var OwnerInfo: TOwnerInfo): Boolean;
begin
  Result := FState = rsSuccess;
  if Result then
    OwnerInfo := FOwnerInfo;
end;

function NextElement(Start, Stop: PByte; var PType, PLength: Byte; var PData: PByte): Boolean;
var
  P, PEnd: PByte;

begin
  P := Start;
  PEnd := Stop;
  while (P <> PEnd) and (P^ <> $5f) do Inc(P);
  Result := (P <> Stop) and (P^ = $5f);
  if Result then
  begin
    Inc(P);
    PType := P^;
    Inc(P);
    PLength := P^;
    Inc(P);
    PData := P;
  end;
end;

function BCD2Byte(BCD: Byte): Byte;
begin
  Result := (BCD shr 4) * 10 + (BCD and $0F);
end;

function ReadDateFromBuffer(Buffer: Pointer; BufferSize: Integer): TDate;
var
  D, M, Y: Integer;
  P: PByte;
begin
  P := Buffer;
  D := BCD2Byte(P^);
  Inc(P);
  M := BCD2Byte(P^);
  Inc(P);
  Y := BCD2Byte(P^);
  Inc(P);
  Y := Y * 100 + BCD2Byte(P^);
  Result := EncodeDate(Y, M, D);
end;

function UTF8ToWideStr(const Buf; Size: Integer): WideString;
var
  Sz: integer;
begin
  if Size <= 0 then
  begin
    Result := '';
    Exit;
  end;

  SetLength(Result, Size);
  Sz := MultiByteToWideChar(CP_UTF8, 0, @Buf, Size, @Result[1], Size);
  SetLength(Result, Sz);
end;

procedure TOMSReader.SetOwnerInfo(AReaderName: String; Buffer: Pointer; BufferSize: Integer);
var
  PType, PLength: Byte;
  PData, Start, Stop: PByte;
  S: String;
begin
  if (BufferSize = 0) and (AReaderName = FReaderName) then
  begin
    Finalize(FOwnerInfo);
    FillChar(FOwnerInfo, SizeOf(FOwnerInfo), 0);
    FReaderName := '';
    FState := rsUnknown;
    if Assigned(FOnSatetChange) then
      FOnSatetChange(Self);
  end
  else if Assigned(Buffer) and (BufferSize > 0) then
  begin
    Start := Buffer;
    Stop := Buffer;
    Inc(Stop, BufferSize);
    while NextElement(Start, Stop, PType, PLength, PData) do
    begin
      case PType of
        $26: // Номер полиса
        begin
          FOwnerInfo.MedicalInsuranceNumber := UTF8ToWideStr(PData^, PLength);
          FOwnerInfo.Policy := FOwnerInfo.MedicalInsuranceNumber;
        end;
        $2a: // Дата выдачи?
          FOwnerInfo.RegistrationDate := ReadDateFromBuffer(PData, PLength);
        $28: // Действует до, может 0, значит не ограничен
          FOwnerInfo.MedicalInsuranceDateEnd := ReadDateFromBuffer(PData, PLength);
        $21: // Фамилия
          FOwnerInfo.LName := UTF8ToWideStr(PData^, PLength);
        $22: // Имя
          FOwnerInfo.FName := UTF8ToWideStr(PData^, PLength);
        $23: // Отчество
          FOwnerInfo.MName := UTF8ToWideStr(PData^, PLength);
        $25: // Пол
        begin
          if PData^ = 1 then
            FOwnerInfo.SexName := 'М'
          else
            FOwnerInfo.SexName := 'Ж';
        end;
        $24: // Дата рождения
          FOwnerInfo.Byrthday := ReadDateFromBuffer(PData, PLength);
        $29: // Место рождения
          FOwnerInfo.BirthPlace := UTF8ToWideStr(PData^, PLength);
        $31: // Код страны
          FOwnerInfo.CountryCode := UTF8ToWideStr(PData^, PLength);
        $32: // Название страны
          FOwnerInfo.Country := UTF8ToWideStr(PData^, PLength);
        $27: // СНИЛС
          FOwnerInfo.InsuranceCertificate := UTF8ToWideStr(PData^, PLength);
        $41: // Формат изображения
          FOwnerInfo.ImgFormat := UTF8ToWideStr(PData^, PLength);
        $42: // Данные фоторгафии, соответствующие ImgFormat
        begin
          SetLength(FOwnerInfo.Img, PLength);
          Move(PData^, FOwnerInfo.Img[1], PLength);
        end;
        $51: // ОГРН для СМО
          FOwnerInfo.OGRN := UTF8ToWideStr(PData^, PLength);
        $52: // ОКАТО для СМО
          FOwnerInfo.OKATO := UTF8ToWideStr(PData^, PLength);
        $53: // Дата начала страхования в СМО
          FOwnerInfo.DateBegin := ReadDateFromBuffer(PData, PLength);
        $54: // Дата окончания страхования в СМО
          FOwnerInfo.DateEnd := ReadDateFromBuffer(PData, PLength);
      end;

      if PLength = 1 then // булевское
        AddLog(ltMessage, 0, 'Type='+IntToHex(PType, 2) + ', Len=' + IntToStr(PLength) + ', Data=' + BoolToStr(PData^ = 1, True))
      else if PLength = 4 then // дата
        AddLog(ltMessage, 0, 'Type='+IntToHex(PType, 2) + ', Len=' + IntToStr(PLength) + ', Data=' + DateToStr(ReadDateFromBuffer(PData, PLength)))
      else // строка
      begin
        S := UTF8ToWideStr(PData^, PLength);
        AddLog(ltMessage, 0, 'Type='+IntToHex(PType, 2) + ', Len=' + IntToStr(PLength) + ', Data=' + S);
      end;
      Start := PData;
      Inc(Start, PLength);
    end;

    FReaderName := AReaderName;
    FState := rsSuccess;
    if Assigned(FOnSatetChange) then
      FOnSatetChange(Self);
  end;
end;

procedure TOMSReader.Transmit(Reader: TReaderObjectExt; Command: Pointer;
  CLen: Integer; var Response: TTransmitResponse);
var
  OutSize: DWORD;
  PCSCResult: DWORD;
  OutBuffer: array of byte;
begin
  OutSize := 0;
  PCSCResult := Reader.SCTransmit(FPCSCRaw, FPCSCDeviceContext, Command, nil, CLen, OutSize);
  if Assigned(FOnLog) then AddLog(ltSend, PCSCResult, BufToString(Command, CLen));
  if PCSCResult = SCARD_S_SUCCESS then
  begin
    SetLength(OutBuffer, OutSize);
    PCSCResult := Reader.SCTransmit(FPCSCRaw, FPCSCDeviceContext, Command, @OutBuffer[0], CLen, OutSize);
    if Assigned(FOnLog) then AddLog(ltRecive, PCSCResult, BufToString(@OutBuffer[0], OutSize));
    if PCSCResult = SCARD_S_SUCCESS then
    begin
      ParseResponse(@OutBuffer[0], OutSize, Response);
    end;
  end;
end;

procedure TOMSReader.UpdatePCSCReaderList;
var
  I, J: integer;
  ReaderName: string;
  ReaderList: TStringList;
  ReaderObject: TReaderObjectExt;
begin
  ReaderList := TStringList.Create;
  try
    GetPCSCReaderList(ReaderList);
    for I := FReaderList.Count - 1 downto 0 do
    begin
      ReaderName := FReaderList.Readers[I].ReaderName;
      J := ReaderList.IndexOf(ReaderName);
      if J < 0 then
      begin
        AddLog(ltMessage, 0,  'Reader removed: ' + ReaderName);
        FReaderList.Delete(I);
      end;
    end;

    for I := 0 to ReaderList.Count - 1 do
    begin
      ReaderName := ReaderList[I];
      J := FReaderList.IndexOfName(ReaderName);
      if J < 0 then
      begin
        ReaderObject := TReaderObjectExt.Create(ReaderName, FPCSCRaw);
        ReaderObject.OnCardStateChanged := CardStateChanged;
        FReaderList.Add(ReaderObject);
        AddLog(ltMessage, 0,  'New reader found: ' + ReaderName);
      end;
    end;
  finally
    ReaderList.Free;
  end;
end;

{ TReaderList }

function TReaderList.GetReader(Index: Integer): TReaderObjectExt;
begin
  Result := Items[Index] as TReaderObjectExt;
end;

function TReaderList.IndexOfName(ReaderName: String): Integer;
var
  I: Integer;
  Reader: TReaderObject;
begin
  Result := -1;
  for I := 0 to Count - 1 do
  begin
    Reader := Readers[I];
    if Reader.ReaderName = ReaderName then
    begin
      Result := I;
      Break;
    end;
  end;
end;

procedure TReaderList.SetReader(Index: Integer;
  const Value: TReaderObjectExt);
begin
  Items[Index] := Value;
end;

{ TReaderObjectExt }

constructor TReaderObjectExt.Create(AReaderName: String;
  PCSCRaw: TPCSCRaw);
begin
  FLockRead := 0;
  FOldCardState := csUnknown;
  FOldCardStateTemp := csUnknown;
  inherited Create(AReaderName, PCSCRaw);
  inherited OnCardStateChanged := DoOnCardStateChanged;
end;

destructor TReaderObjectExt.Destroy;
begin

  inherited;
end;

procedure TReaderObjectExt.DoOnCardStateChanged(Sender: TObject);
begin
  FOldCardState := FOldCardStateTemp;
  FOldCardStateTemp := CardState;
  if Assigned(FOnCardStateChanged) then
    FOnCardStateChanged(Self);
end;

function TReaderObjectExt.IsLockRead: Boolean;
begin
  Result := FLockRead > 0;
end;

procedure TReaderObjectExt.LockRead;
begin
  Inc(FLockRead);
end;

procedure TReaderObjectExt.UnLockRead;
begin
  Dec(FLockRead);
  if FLockRead < 0 then FLockRead := 0;
end;

end.
