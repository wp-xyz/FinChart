unit fcmain;

{ on downloading stock prices, see
  https://code.google.com/p/yahoo-finance-managed/wiki/csvHistQuotesDownload
}

{ test: https://finance.google.com/finance?output=json&q=  }

{$mode objfpc}{$H+}

interface

uses
  // RTL/FCL
  Classes, SysUtils, Types,
  // LazUtils
  FileUtil,
  // LCL
  LCLIntf, Graphics, Forms, Controls, Dialogs, ExtCtrls,
  StdCtrls, EditBtn, ComCtrls, Grids, Buttons,
  // TAChart
  TAGraph, TACustomSeries, TASeries, TAMultiSeries, TAChartExtentLink,
  TAIntervalSources, TAChartListbox, TATransformations, TAFuncSeries, TASources,
  TATools, TALegend, TACustomSource, TANavigation;

type

  { TMainForm }

  TMainForm = class(TForm)
    ApplicationProperties: TApplicationProperties;
    Bevel1: TBevel;
    BtnEditStockItem: TSpeedButton;
    BtnDeleteStockItem: TSpeedButton;
    BtnLoad: TButton;
    BtnAddStockItem: TSpeedButton;
    BtnURL: TButton;
    CbShowVolumeChart: TCheckBox;
    CbProvider: TComboBox;
    DataPointCrosshairTool: TDataPointCrosshairTool;
    DateTimeIntervalChartSource: TDateTimeIntervalChartSource;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    lblOpen: TLabel;
    lblDate: TLabel;
    lblHigh: TLabel;
    lblLow: TLabel;
    lblClose: TLabel;
    lblVolume: TLabel;
    LblTickerSymbol: TLabel;
    lblProvider: TLabel;
    MoveAveSeries50: TLineSeries;
    MoveAreSeries200: TLineSeries;
    OHLCSeries: TOpenHighLowCloseSeries;
    ChartListboxPanel: TPanel;
    Panel1: TPanel;
    ChartListbox: TChartListbox;
    ChartToolset: TChartToolset;
    PanDragTool: TPanDragTool;
    VolumeSeries: TAreaSeries;
    ZoomDragTool: TZoomDragTool;
    MovingAverageSource200: TCalculatedChartSource;
    MovingAverageSource50: TCalculatedChartSource;
    RawData: TMemo;
    ChartExtentLink: TChartExtentLink;
    NavScrollBar: TChartNavScrollBar;
    ChartPanel: TPanel;
    CbStockItems: TComboBox;
    DataGrid: TDrawGrid;
    EdEndDate: TDateEdit;
    LblEndDate: TLabel;
    OHLCChart: TChart;
    EdStartDate: TDateEdit;
    lblStock: TLabel;
    LblStartDate: TLabel;
    LeftPanel: TPanel;
    PageControl: TPageControl;
    RgInterval: TRadioGroup;
    Splitter1: TSplitter;
    ChartSplitter: TSplitter;
    OHLCSource: TUserDefinedChartSource;
    PgSheet: TTabSheet;
    PgTable: TTabSheet;
    PgRaw: TTabSheet;
    ChartListboxSplitter: TSplitter;
    VolumeSource: TUserDefinedChartSource;
    VolumeChart: TChart;
    procedure ApplicationPropertiesException(Sender: TObject; E: Exception);
    procedure BtnEditStockItemClick(Sender: TObject);
    procedure BtnAddStockItemClick(Sender: TObject);
    procedure BtnDeleteStockItemClick(Sender: TObject);
    procedure BtnLoadClick(Sender: TObject);
    procedure BtnURLClick(Sender: TObject);
    procedure CbShowVolumeChartChange(Sender: TObject);
    procedure CbProviderSelect(Sender: TObject);
    procedure CbStockItemsSelect(Sender: TObject);
    procedure ChartListboxAddSeries(ASender: TChartListbox;
      ASeries: TCustomChartSeries; AItems: TChartLegendItems; var ASkip: Boolean);
    procedure ChartListbox1CheckboxClick(ASender: TObject; AIndex: Integer);
    procedure DataGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure DataGridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure DataPointCrosshairToolDraw(ASender: TDataPointDrawTool);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure OHLCChartAfterDrawBackWall(ASender: TChart; ACanvas: TCanvas;
      const ARect: TRect);
    procedure OHLCSourceGetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
    procedure VolumeSourceGetChartDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
  private
    { private declarations }
    FChartRect: TRect;
    FmtSettings: TFormatSettings;
    procedure AddSeries(AList: TStringList; AStockName, ATickerSymbol, AStockCurrency: String);
    procedure AdjustColWidths;
    function GetCellText(ACol, ARow: Integer): String;
    procedure PopulateSourceCombo;
    procedure PopulateStockCombo;
    procedure ReadIni;
    function SelectedStockCompany: String;
    function SelectedStockCurrency: String;
    procedure UpdateTickerSymbol;
    procedure WriteIni;
  public
    { public declarations }
    procedure BeforeRun;
  end;

var
  MainForm: TMainForm;


implementation

{$R *.lfm}

uses
 {$IFDEF MSWINDOWS}
  windows, wininet,
 {$ELSE}
  {$IF FPC_FullVersion >= 30200}opensslsockets,{$IFEND}
  fphttpclient,
 {$ENDIF}
  inifiles, LazFileUtils, DateUtils, TypInfo,
  TAChartUtils, TAChartAxisUtils, 
  fcStockItemForm, fcProviders, fcStockItems;


{ Plot data }
type
  TStockDataRec = record
    DateValue: TDateTime;
    OpenValue,
    HighValue,
    LowValue,
    CloseValue: Double;
    VolumeValue: Double;
  end;

  TStockDataArray = array of TStockDataRec;

var
  StockData: TStockDataArray = nil;


{ Ini file }

function CreateIni: TCustomIniFile;
const
  NOT_GLOBAL = false;
  NO_SUBDIR = false;
  CREATE_DIR = true;
var
  fn: String;
begin
  fn := GetAppConfigFileUTF8(NOT_GLOBAL, NO_SUBDIR, CREATE_DIR);
  Result := TMemIniFile.Create(ChangeFileExt(fn, '.ini'));
end;


{ Download from internet }

{$IFDEF MSWINDOWS}
{ This function translates a WinInet Error Code to a description of the error.
  From: https://theroadtodelphi.com/category/wininet/ }
function GetWinInetError(ErrorCode: Cardinal): string;
const
  winetdll = 'wininet.dll';
var
  len: Integer;
  buffer: PChar;
begin
  len := FormatMessage(
    FORMAT_MESSAGE_FROM_HMODULE or FORMAT_MESSAGE_FROM_SYSTEM or
    FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_IGNORE_INSERTS or FORMAT_MESSAGE_ARGUMENT_ARRAY,
    {%H-}Pointer(GetModuleHandle(winetdll)), ErrorCode, 0, @buffer, SizeOf(Buffer), nil
  );
  try
    while (len > 0) and (Buffer[len - 1] in [#0..#32, '.']) do
      dec(len);
    SetString(Result, buffer, len);
  finally
    LocalFree({%H-}HLOCAL(buffer));
  end;
end;

// Adapted from
//   http://www.scalabium.com/faq/dct0080.htm
function DownloadHTTP(URL: String; AStream: TStream; out AErrMsg: String): Boolean;
const
  KB = 1024;
var
  netHandle: HInternet;
  urlHandle: HInternet;
  buffer: array[0..4*KB-1] of Byte;
  bytesRead: dWord = 0;
  errCode: Integer = 0;
begin
  Result := false;
  AErrMsg := '';
  NetHandle := InternetOpen('Mozilla/5.0(compatible; WinInet)', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);

  // NetHandle valid?
  if netHandle = nil then
  begin
    errCode := GetLastError;
    AErrMsg := GetWinInetError(errCode);
    exit;
  end;

  try
    urlHandle := InternetOpenUrl(netHandle, PChar(URL), nil, 0, INTERNET_FLAG_RELOAD, 0);
    if urlHandle = nil then
    begin
      errCode := GetLastError;
      AErrMsg := GetWinInetError(errCode);
      exit;
    end;

    try
      repeat
        InternetReadFile(urlHandle, @buffer, SizeOf(buffer), bytesRead);
        if bytesRead > 0 then
          AStream.Write(buffer, bytesRead);
      until bytesRead = 0;
      AStream.Position := 0;
      Result := true;
    finally
      InternetCloseHandle(urlHandle);
    end
  finally
    InternetCloseHandle(netHandle);
  end;
end;
{$ELSE}
// Get file from the internet
function DownloadHTTP(URL: String; AStream: TStream; out AErrMsg: String): Boolean;
begin
  AErrMsg := '';
  with TFpHttpClient.Create(nil) do
    try
      try
        AllowRedirect := true;
        Get(URL, AStream);
        AStream.Position := 0;
        Result := true;
      except
        on E:EHTTPClient do begin
          AErrMsg := E.Message;
          Result := false;
        end;
      end;
    finally
      Free;
    end;
end;
{$ENDIF}


{ TMainForm }

procedure TMainForm.AddSeries(AList: TStringList;
  AStockName, ATickerSymbol, AStockCurrency: String);
var
  L: TStringList;
  i, j: Integer;
  clr: TColor;
  rndSeed: Integer;
  fs: TFormatSettings;
  x: Double;
  d: TDate;
  provider: TProvider;
begin
  if CbProvider.ItemIndex = -1 then
    exit;
  provider := GetProvider(CbProvider.ItemIndex);

  fs := FmtSettings;
  fs.ShortDateFormat := provider.CSVDateFormat;
  fs.DateSeparator := provider.CSVDateSeparator;

  L := TStringList.Create;
  try
    L.Delimiter := ',';
    j := 0;
    SetLength(StockData, AList.Count);
    for i:=AList.Count-1 downto 1 do begin
      if AList[i] = '' then
        Continue;
      L.CommaText := AList[i];
      if not TryStrToDate(L[0], d, fs) then
      begin
        MessageDlg('Incorrect data. Date value expected as first field. ' + LineEnding +
                   'Have a look at the "Raw" tab for further information.',
                   mtError, [mbOK], 0);
        exit;
      end;
      StockData[j].DateValue := d; //ScanDateTime(provider.CSVDateFormat, L[0], fs);
      if TryStrToFloat(L[1], x, FmtSettings) and (x > 0) then
        StockData[j].OpenValue := x else
        Continue;
      if TryStrToFloat(L[2], x, FmtSettings) and (x > 0) then
        StockData[j].HighValue := x else
        Continue;
      if TryStrToFloat(L[3], x, FmtSettings) and (x > 0) then
        StockData[j].LowValue := x else
        Continue;
      if TryStrToFloat(L[4], x, FmtSettings) and (x > 0) then
        StockData[j].CloseValue := x else
        Continue;
      // Some queries (new Yahoo) have 7 columns, some 6. Volume is always the last.
      if TryStrToFloat(L[L.Count-1], x, FmtSettings) and (x > 0) then
        StockData[j].VolumeValue := x else
        Continue;
      inc(j);
    end;
    SetLength(StockData, j);

    OHLCSource.PointsNumber := Length(StockData);
    OHLCSource.Reset;

    VolumeSource.PointsNumber := Length(StockData);
    VolumeSource.Reset;

    if PageControl.ActivePage = PgTable then
      DataGrid.Invalidate;
  finally
    L.Free;
  end;

  DataGrid.RowCount := DataGrid.FixedRows + Length(StockData);
  AdjustColWidths;

  rndSeed := RandSeed;
  try
    RandSeed := 0;
    clr := rgb(Random(256), Random(256), Random(256));
  finally
    RandSeed := rndSeed;
  end;

  OHLCSeries.Title := AStockname;
  OHLCSeries.Mode := mCandleStick;
  OHLCSeries.CandlestickLinePen.Color := clDefault;

  OHLCChart.Title.Text.Text := Format('%s (%s)', [AStockName, ATickerSymbol]);
  OHLCChart.Title.Visible := true;
  OHLCChart.LeftAxis.Title.Caption := 'Stock price, ' + AStockCurrency;
  OHLCChart.ZoomFull;

  VolumeSeries.Title := AStockName + ' (Volume)';
  VolumeSeries.SeriesColor := clr;
  VolumeSeries.AreaLinesPen.Style := psClear;
  VolumeSeries.AreaContourPen.Style := psClear;
  VolumeChart.ZoomFull;

  ChartListboxPanel.Visible := true;
  ChartListboxSplitter.Visible := true;
  ChartListboxSplitter.Left := 0;
end;

procedure TMainForm.AdjustColWidths;
var
  w: Integer;
  i: Integer;
  bmp: Graphics.TBitmap;
begin
  bmp := Graphics.TBitmap.Create;
  try
    bmp.SetSize(10, 10);
    bmp.Canvas.Font.Assign(DataGrid.Font);
    w := bmp.Canvas.TextWidth('  9.999,99  ');
    for i := 2 to DataGrid.ColCount-2 do
      DataGrid.ColWidths[i] := w;
    DataGrid.ColWidths[0] := bmp.Canvas.TextWidth('  99999  ');
    DataGrid.ColWidths[1] := bmp.Canvas.TextWidth('    Mo 99.99.9999    ');
    DataGrid.ColWidths[DataGrid.ColCount-1] := bmp.Canvas.TextWidth(' 999.999.999.999.999 ');
  finally
    bmp.Free;
  end;
end;

procedure TMainForm.ApplicationPropertiesException(Sender: TObject;
  E: Exception);
begin
  MessageDlg(E.Message, mtError, [mbOK], 0);
end;

procedure TMainForm.BeforeRun;
begin
  ReadIni;
end;

procedure TMainForm.BtnAddStockItemClick(Sender: TObject);
var
  F: TStockItemForm;
  P: TObject;
begin
  F := TStockItemForm.Create(nil);
  try
    if F.ShowModal = mrOK then begin
      RegisterStockItem(F.EdCompany.Text, F.TickerSymbols, F.EdCurrency.Text);
      PopulateStockCombo;
      CbStockItems.ItemIndex := CbStockItems.Items.IndexOf(F.EdCompany.Text);
    end;
  finally
    F.Free;
  end;
end;

procedure TMainForm.BtnDeleteStockItemClick(Sender: TObject);
begin
  if CbStockItems.ItemIndex = -1 then
    exit;
  if MessageDlg('Do you really want to delete this stock item?', mtConfirmation,
    [mbYes, mbNo, mbCancel], 0) <> mrYes then exit;
  DeleteStockItem(SelectedStockCompany);
  PopulateStockCombo;
end;

procedure TMainForm.BtnEditStockItemClick(Sender: TObject);
var
  F: TStockItemForm;
  P: TObject;
  item: TStockItem;
begin
  if CbStockItems.ItemIndex < 0 then
    exit;

  item := FindStockItem(SelectedStockCompany);
  F := TStockItemForm.Create(nil);
  try
    F.EdCompany.Text := item.Company;
    F.TickerSymbols := item.WriteTickerSymbols;
    F.EdCurrency.Text := item.Currency;
    if F.ShowModal = mrOK then begin
      DeleteStockItem(item.Company);
      RegisterStockItem(F.EdCompany.Text, F.TickerSymbols, F.EdCurrency.Text);
      PopulateStockCombo;
      CbStockItems.ItemIndex := CbStockItems.Items.IndexOf(F.EdCompany.Text);
      UpdateTickerSymbol;
    end;
  finally
    F.Free;
  end;
end;

procedure TMainForm.BtnLoadClick(Sender: TObject);
// see on how to download historical stock prices from finance.yahoo:
// https://code.google.com/p/yahoo-finance-managed/wiki/csvHistQuotesDownload
// http://www.quantshare.com/sa-426-6-ways-to-download-free-intraday-and-tick-data-for-the-us-stock-market

const
  INTERVALS: array[0..2] of String = ('d', 'w', 'm');

  YAHOO_URL = 'https://ichart.yahoo.com/table.csv?s=%s&a=%d&b=%d&c=%d&d=%d&e=%d&f=%d&g=%s&ignore=.csv';
  // Parameters:
  // s = stock ticker symbol
  // a = start month (zero-based), b = start day, c = start year
  // d = end month (zero-based), e: end day, f: end year
  // g = interval ("w" = week, "d" = day)
  // Example:
  // http://ichart.finance.yahoo.com/table.csv?s=GOOG&a=00&b=1&c=2000&d=09&e=7&f=2014&g=d&ignore=.csv

(*
  // http://www.google.com/finance/historical?q=GOOG&output=csv

  GOOGLE_URL = 'http://www.google.com/finance/historical?q=%s&startdate=%s&enddate=%s&output=csv';
  // Parameters:
  // q = ticker symbol of the stock (symbol lookup: http://www.google.com/finance)
  // startdate in format like "Nov 1, 2011" (without quotes)
  // enddate, dto.
  // Example:
  // http://www.google.com/finance/historical?q=AAPL&startdate=Nov 1, 2011&enddate=Nov 30, 2011&output=csv
  {
  GOGGLE_URL = 'http://www.google.com/finance/getprices?i=%d&p=%dd&f=d,o,h,l,c,v&df=cpct&q=%s';
  // Parameters:
  // i = interval or frequency in seconds
  // p = historical data period, where "10d" means that we need historical stock prices data for the past 10 days.
  // q = ticker symbol of the stock (symbol lookup: http://www.google.com/finance)
  // Example:
  // http://www.google.com/finance/getprices?i=60&p=10d&f=d,o,h,l,c,v&df=cpct&q=IBM
   }

  MSN_MASK = 'http://moneycentral.msn.com/investor/charts/chartdl.aspx?D4=1&DD=1&MA0=0&MA1=0&CF=0&PT=7&SZ=0&D5=0&'+
    'DCS=2&C1=0&C2=1&width=612&height=258&D2=0&CE=0&'+
    'filedownloadbt.x=59&filedownloadbt.y=12&symbol=[Symbol name]';

  // www.alphavantage.co
  // the phrase after "apikey" is the ApiKey - to get it register at https://www.alphavantage.co/support/#api-key
  ALPHAVANTAGE_URL = 'https://www.alphavantage.co/query?function=TIME_SERIES_MONTHLY' +
    '&datatype=csv&symbol=%s' +
    '&apikey=...';
          *)
var
  stream: TStream;
  url: String;
  y1,m1,d1: Word;
  y2,m2,d2: Word;
  stockItem: TStockItem;
  stockSymbol: String;
  stockname: String;
  stockcurrency: String;
  interval: String;
  startDateStr: String;
  endDateStr: String;
  L: TStringList;
  msg: String;
  idx, p: Integer;
  provider: TProvider;
  resolution: string;
begin
  if CbProvider.ItemIndex = -1 then
  begin
    MessageDlg('Please select a stock data provider.', mtInformation, [mbOK], 0);
    exit;
  end;
  provider := GetProvider(CbProvider.ItemIndex);

  if CbStockItems.ItemIndex = -1 then
  begin
    MessageDlg('Please select a stock.', mtInformation, [mbOK], 0);
    exit;
  end;
  stockItem := FindStockItem(SelectedStockCompany);

  if provider.AllowDateRange then
  begin
    if EdStartDate.Text = '' then
    begin
      MessageDlg('Please select a start date.', mtInformation, [mbOK], 0);
      exit;
    end;
    if EdEndDate.Text = '' then
      EdEndDate.Date := now;
  end;

  if provider.AllowResolution then
  begin
    idx := RgInterval.ItemIndex;
    if not TRadioButton(RgInterval.Components[idx]).Enabled then
    begin
      MessageDlg('Selected interval not available. Please select another interval.', mtInformation, [mbOK], 0);
      exit;
    end;
  end;

  stockname := stockItem.Company;
  stockCurrency := stockItem.Currency;
  stockSymbol := stockItem.TickerSymbol[provider.Name];

  if provider.NeedsApiKey and (provider.ApiKey = '') then begin
    provider.ApiKey := InputBox('Enter API key provided by ' + provider.Name, 'API Key:', '');
    if provider.ApiKey = '' then
      exit;
  end;

  if provider.URLDateFormat = 'unix' then
  begin
    startDateStr := IntToStr(DateTimeToUnix(EdStartDate.Date));
    endDateStr := IntToStr(DateTimeToUnix(EdEndDate.Date));
  end else
  begin
    startDateStr := FormatDateTime(provider.URLDateFormat, EdStartDate.Date, FmtSettings);
    endDateStr := FormatDateTime(provider.URLDateFormat, EdEndDate.Date, FmtSettings);
  end;
  resolution := provider.Resolutions[TResolution(RgInterval.ItemIndex)];
  url := Format(provider.URL, [stockSymbol, provider.ApiKey, startDateStr, endDateStr, resolution]); // do not change order!

  Screen.BeginWaitCursor;
  stream := TMemoryStream.Create;
  L := TStringList.Create;
  try
    RawData.Lines.Clear;
    RawData.Lines.Add(url);
    RawData.Lines.Add('');
    if DownloadHTTP(url, stream, msg) then begin
      stream.Position := 0;
      L.LoadFromStream(stream);
      RawData.Lines.Add(L.Text);
      if provider.PrepareData(L) then
        AddSeries(L, stockname, stocksymbol, stockcurrency)
      else
        PageControl.ActivePage := PgRaw;
    end else begin
      RawData.Lines.Add('Could not get data: '+msg);
      PageControl.ActivePage := PgRaw;
    end;
  finally
    L.Free;
    stream.Free;
    Screen.EndWaitCursor;
  end;
end;

procedure TMainForm.BtnURLClick(Sender: TObject);
var
  url: String;
  stream: TMemoryStream;
  L: TStrings;
  msg: String;
begin
  url := Inputbox('Enter URL', 'URL:', '');
  if url <> '' then begin
    Screen.Cursor := crHourglass;
    stream := TMemoryStream.Create;
    L := TStringList.Create;
    try
      RawData.Lines.Clear;
      RawData.Lines.Add(url);
      RawData.Lines.Add('');
      if DownloadHTTP(url, stream, msg) then begin
        stream.Position := 0;
        L.LoadFromStream(stream);
        RawData.Lines.Add(L.Text);
      end else
        RawData.Lines.Add('Could not get data: '+msg);
      PageControl.ActivePage := PgRaw;
    finally
      L.Free;
      stream.Free;
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TMainForm.CbShowVolumeChartChange(Sender: TObject);
begin
  VolumeChart.Visible := CbShowVolumeChart.Checked;
  ChartSplitter.Visible := CbShowVolumeChart.Checked;
  ChartSplitter.Top := 0;
end;

procedure TMainForm.CbProviderSelect(Sender: TObject);
var
  provider: TProvider;
begin
  UpdateTickerSymbol;
  provider := GetProvider(CbProvider.ItemIndex);
  if provider.AllowResolution then
  begin
    TRadioButton(RgInterval.Components[0]).Enabled := provider.Resolutions[rDaily] <> '';
    TRadioButton(RgInterval.Components[1]).Enabled := provider.Resolutions[rWeekly] <> '';
    TRadioButton(RgInterval.Components[2]).Enabled := provider.Resolutions[rMonthly] <> '';
  end;
  RgInterval.Visible := provider.AllowResolution;
  EdEndDate.Visible := provider.AllowDateRange;
  EdStartDate.Visible := provider.AllowDateRange;
  lblEndDate.Visible := provider.AllowDateRange;
  lblStartDate.Visible := provider.AllowDateRange;
end;

procedure TMainForm.CbStockItemsSelect(Sender: TObject);
begin
  UpdateTickerSymbol;
end;

procedure TMainForm.ChartListboxAddSeries(ASender: TChartListbox;
  ASeries: TCustomChartSeries; AItems: TChartLegendItems; var ASkip: Boolean);
begin
  if ASeries is TConstantLine then
    ASkip := true;
end;

procedure TMainForm.ChartListbox1CheckboxClick(ASender: TObject; AIndex: Integer);
var
  ser: TBasicChartSeries;
begin
  ser := VolumeChart.Series[AIndex];
  OHLCChart.Series[ser.Tag].Active := ser.Active;
end;

procedure TMainForm.DataGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  s: String;
begin
  s := GetCellText(ACol, ARow);
  if s <> '' then begin
    InflateRect(ARect, -varCellPadding, -varCellPadding);
    DataGrid.Canvas.TextRect(ARect, ARect.Left, ARect.Top, s);
  end;
end;

procedure TMainForm.DataGridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
var
  ts: TTextStyle;
begin
  ts := DataGrid.Canvas.TextStyle;
  if (ACol = 1) or (ARow = 0) then
    ts.Alignment := taCenter
  else
    ts.Alignment := taRightJustify;
  ts.Opaque := false;
  if ARow = 0 then
    DataGrid.Canvas.Font.Style := [fsBold];
  DataGrid.Canvas.TextStyle := ts;
end;

procedure TMainForm.DataPointCrosshairToolDraw(ASender: TDataPointDrawTool);
var
  index: Integer;
  curr: String;
  fmt: String;
  provider: TProvider;
begin
  if (ASender <> nil) and (ASender.PointIndex > -1) then
  begin
    index := ASender.PointIndex;
    curr := SelectedStockCurrency;
    provider := GetProvider(CbProvider.ItemIndex);
    fmt := 'dddd, ddddd';
    if provider.AllowDateRange then
      case RgInterval.ItemIndex of
        2: fmt := 'mmmm yyyy';
      end;
    lblDate.Caption := FormatDateTime(fmt, StockData[index].DateValue);
    lblOpen.Caption := Format('%.2n %s', [StockData[index].OpenValue, curr]);
    lblHigh.Caption := Format('%.2n %s', [StockData[index].HighValue, curr]);
    lblLow.Caption := Format('%.2n %s', [StockData[index].LowValue, curr]);
    lblClose.Caption := Format('%.2n %s', [StockData[index].CloseValue, curr]);
    lblVolume.Caption := Format('%.0n', [StockData[index].VolumeValue]);
    ASender.Handled;
  end else
  begin
    lblDate.Caption := '';
    lblOpen.Caption := '';
    lblHigh.Caption := '';
    lblLow.Caption := '';
    lblClose.Caption := '';
    lblVolume.Caption := '';
  end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if CanClose then
    try
      WriteIni;
    except
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  FmtSettings := DefaultFormatSettings;
  FmtSettings.DecimalSeparator:= '.';
  FmtSettings.ShortMonthNames[1] := 'Jan';;
  FmtSettings.ShortMonthNames[2] := 'Feb';;
  FmtSettings.ShortMonthNames[3] := 'Mar';;
  FmtSettings.ShortMonthNames[4] := 'Apr';;
  FmtSettings.ShortMonthNames[5] := 'May';;
  FmtSettings.ShortMonthNames[6] := 'Jun';;
  FmtSettings.ShortMonthNames[7] := 'Jul';;
  FmtSettings.ShortMonthNames[8] := 'Aug';;
  FmtSettings.ShortMonthNames[9] := 'Sep';;
  FmtSettings.ShortMonthNames[10] := 'Oct';;
  FmtSettings.ShortMonthNames[11] := 'Nov';;
  FmtSettings.ShortMonthNames[12] := 'Dec';;

  PageControl.ActivePageIndex := 0;
  EdStartDate.Date := EncodeDate(1990, 1, 1);
  EdEndDate.Date := Date();
  PopulateSourceCombo;
  PopulateStockCombo;
  OHLCSource.YCount := 4;
  ChartPanel.Color := clWhite;
  OHLCChart.ParentColor := true;
  VolumeChart.ParentColor := true;
  RgInterval.Controls[2].BorderSpacing.Bottom := 6;
  ChartExtentLink.AlignSides := [calLeft];
  DataPointCrosshairToolDraw(nil);

  for i:= 0 to RgInterval.ComponentCount-1 do
    TRadioButton(RgInterval.Components[i]).ParentFont := false;
  RgInterval.Font.Style := [fsItalic];
end;

function MyDateToStr(ADate: TDate): String;
var
  s: String;
begin
  Result := Format('%s %s', [
    FormatDateTime('ddd', ADate),
    DateToStr(ADate)
  ]);
end;

function TMainForm.GetCellText(ACol, ARow: Integer): String;
var
  i: Integer;
begin
  if ARow = 0 then
    case ACol of
      0: ;
      1: Result := 'Date';
      2: Result := 'Open';
      3: Result := 'High';
      4: Result := 'Low';
      5: Result := 'Close';
      6: Result := 'Volume';
    end
  else
  begin
    i := ARow - DataGrid.FixedRows;
    if (i >= 0) and (i < Length(StockData)) then
      case ACol of
        0: ;
        1: Result := MyDateToStr(StockData[i].DateValue);
        2: Result := Format('%.2f', [StockData[i].OpenValue]);
        3: Result := Format('%.2f', [StockData[i].HighValue]);
        4: Result := Format('%.2f', [StockData[i].LowValue]);
        5: Result := Format('%.2f', [StockData[i].CloseValue]);
        6: Result := Format('%.0n', [StockData[i].VolumeValue]);
      end;
  end;
end;

procedure TMainForm.OHLCChartAfterDrawBackWall(ASender: TChart;
  ACanvas: TCanvas; const ARect: TRect);
begin
  FChartRect := ARect;
end;

procedure TMainForm.OHLCSourceGetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  AItem.X := StockData[AIndex].DateValue;
  AItem.Y := StockData[AIndex].LowValue;
  AItem.YList[0] := StockData[AIndex].OpenValue;
  AItem.YList[1] := StockData[AIndex].CloseValue;
  AItem.YList[2] := StockData[AIndex].HighValue;
  AItem.Text := DateToStr(StockData[AIndex].DateValue);
end;

procedure TMainForm.PopulateSourceCombo;
begin
  CbProvider.Items.BeginUpdate;
  try
    ListProviders(CbProvider.Items);
  finally
    CbProvider.Items.EndUpdate;
  end;
end;

procedure TMainForm.PopulateStockCombo;
begin
  ListStockItemCompanies(CbStockItems.Items);
end;

procedure TMainForm.ReadIni;
var
  ini: TCustomIniFile;
  W, H, L, T: Integer;
  winState: Integer;
  lines, items: TStringList;
  sname, svalue: String;
  d: TDate;
begin
  ini := CreateIni;
  try
    L := ini.ReadInteger('MainForm', 'Left', Left);
    T := ini.ReadInteger('MainForm', 'Top', Top);
    W := ini.ReadInteger('MainForm', 'Width', Width);
    H := ini.ReadInteger('MainForm', 'Height', Height);
    winState := ini.ReadInteger('MainForm', 'WindowState', ord(WindowState));
    SetBounds(L, T, W, H);
    WindowState := TWindowState(winState);

    VolumeChart.Visible := ini.ReadBool('MainForm', 'VolumeChartVisible', VolumeChart.Visible);
    VolumeChart.Height := ini.ReadInteger('MainForm', 'VolumeChartHeigt', VolumeChart.Height);
    ChartListboxPanel.Width := ini.ReadInteger('MainForm', 'ChartListboxWidth', ChartlistboxPanel.Width);

    lines := TStringList.Create;
    try
      ini.ReadSection('Providers', lines);
      for sname in lines do begin
        svalue := ini.ReadString('Providers', sname, '');
        if svalue <> '' then
          SetProviderAPIKey(sname, svalue);
      end;
    finally
      lines.Free;
    end;

    lines := TStringList.Create;
    items := TStringList.Create;
    try
      ini.ReadSection('StockItems', lines);
      if lines.Count > 0 then
      begin
        ClearStockItems;
        items.Delimiter := ';';
        items.StrictDelimiter := true;
        for sname in lines do
        begin
          svalue := ini.ReadString('StockItems', sname, '');
          if svalue <> '' then begin
            items.DelimitedText := svalue;
            while items.Count < 2 do items.Add('');
            RegisterStockItem(sname, items[0], items[1]);
          end;
        end;
      end;
    finally
      lines.Free;
    end;

    PopulateStockCombo;

    sname := ini.ReadString('Settings', 'StockItem', '');
    if sname <> '' then
      CbStockItems.ItemIndex := CbStockItems.Items.Indexof(sname);

    sname := ini.ReadString('Settings', 'Provider', '');
    if sname <> '' then
      CbProvider.ItemIndex := GetProviderIndex(sname);

    sname := ini.ReadString('Settings', 'Resolution', '');
    if sname <> '' then
      RgInterval.ItemIndex := GetEnumValue(TypeInfo(TResolution), 'r' + sname)
    else
      RgInterval.ItemIndex := 0;

    if TryStrToDate(ini.ReadString('Settings', 'StartDate', DateToStr(EdStartDate.Date, FmtSettings)), d) then
      EdStartDate.Date := d;

    if TryStrToDate(ini.ReadString('Settings', 'EndDate', DateToStr(EdEndDate.Date, FmtSettings)), d) then
      EdEndDate.Date := d;

    CbProviderSelect(nil);

  finally
    ini.Free;
  end;
end;

function TMainForm.SelectedStockCompany: String;
var
  idx: Integer;
  item: TStockItem;
begin
  Result := '';
  idx := CbStockItems.ItemIndex;
  if idx <> -1 then
  begin
    item := FindStockItem(CbStockItems.Items[idx]);
    if item <> nil then
      Result := item.Company;
  end;
end;

function TMainForm.SelectedStockCurrency: String;
var
  idx: Integer;
  item: TStockItem;
begin
  Result := '';
  idx := CbStockItems.ItemIndex;
  if idx <> -1 then
  begin
    item := FindStockItem(CbStockItems.Items[idx]);
    if item <> nil then
      Result := item.Currency;
  end;
end;

procedure TMainForm.UpdateTickerSymbol;
var
  item: TStockItem;
begin
  if CbStockItems.ItemIndex = -1 then
    exit;
  item := FindStockItem(SelectedStockCompany);
//  item := TStockItem(StockItems[CbStockitems.itemIndex]);
  LblTickerSymbol.Caption := item.TickerSymbol[CbProvider.Text];
  LblTickerSymbol.Show;
end;

procedure TMainForm.VolumeSourceGetChartDataItem(
  ASource: TUserDefinedChartSource; AIndex: Integer; var AItem: TChartDataItem);
begin
  AItem.X := StockData[AIndex].DateValue;
  AItem.Y := StockData[AIndex].VolumeValue;
  AItem.Text := DateToStr(StockData[AIndex].DateValue);
end;

procedure TMainForm.WriteIni;
var
  ini: TCustomIniFile;
  i: Integer;
  s: String;
begin
  ini := CreateIni;
  try
    if WindowState = wsNormal then begin
      ini.WriteInteger('MainForm', 'Left', Left);
      ini.WriteInteger('MainForm', 'Top', Top);
      ini.WriteInteger('MainForm', 'Width', Width);
      ini.WriteInteger('MainForm', 'Height', Height);
    end;
    ini.WriteInteger('MainForm', 'WindowState', ord(WindowState));
    ini.WriteBool('MainForm', 'VolumneChartVisible', VolumeChart.Visible);
    ini.WriteInteger('MainForm', 'VolumeChartHeigt', VolumeChart.Height);
    if ChartListboxPanel.Visible then
      ini.WriteInteger('MainForm', 'ChartListboxWidth', ChartlistboxPanel.Width);

    for i:=0 to NumProviders-1 do
      with GetProvider(i) do
        ini.WriteString('Providers', Name, ApiKey);

    for i:=0 to NumStockItems-1 do
      with GetStockItem(i) do
        ini.WriteString('StockItems', Company, WriteTickerSymbols + ';' + Currency);

    ini.WriteString('Settings', 'StartDate', DateToStr(EdStartDate.Date, FmtSettings));
    ini.WriteString('Settings', 'EndDate', DateToStr(EdEndDate.Date, FmtSettings));
    ini.WriteString('Settings', 'Provider', CbProvider.Items[CbProvider.ItemIndex]);
    ini.WriteString('Settings', 'StockItem', CbStockItems.Items[CbStockItems.ItemIndex]);
    s := GetEnumName(TypeInfo(TResolution), RgInterval.ItemIndex);
    Delete(s, 1, 1);
    ini.WriteString('Settings', 'Resolution', s);
  finally
    ini.Free;
  end;
end;


initialization
  CreateProviders;

finalization
  FreeProviders;

end.

