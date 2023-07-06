unit fcProviders;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

{ Providers }

type
  TResolution = (rDaily, rWeekly, rMonthly);

  TProvider = class
    ID: Integer;
    { Name of the stock data provider appearing in the CbProvider combobox }
    Name: String;
    { URL for the download of the stock prices; uses the following parameters at
      these indexes
        0: Ticker symbol
        1: API key
        2: Start date as string
        3: End date as string
        4: ResolutionType (daily, weeky, monthly)
    }
    URL: String;
    { Allows resolution }
    AllowResolution: Boolean;
    { Names for time resolution of data }
    Resolutions: array[TResolution] of string;
    { Allows start date to end date range }
    AllowDateRange: Boolean;
    { Format for dates inserted in the URL }
    URLDateFormat: String;
    { Format mask describing how dates are contained in the csv file }
    CSVDateFormat: String;
    { Date separator used in CSV date string }
    CSVDateSeparator: char;
    { If true an API key is required by the provider. }
    NeedsApiKey: Boolean;
    { API key sent from the provider. Needed only if NeedsApiKey is true. }
    APIKey: String;

    constructor Create; virtual;
    { Procedure to standardize the data received from the web-service }
    function PrepareData(AList: TStrings): Boolean; virtual;
  end;

  TAlphaVantageProvider = class(TProvider)
  public
    constructor Create; override;
  end;

  TMarketStackProvider = class(TProvider)
  public
    constructor Create; override;
    function PrepareData(AList: TStrings): Boolean; override;
  end;

  TYahooFinanceProvider = class(TProvider)
  public
    constructor Create; override;
  end;

procedure CreateProviders;
function GetProvider(AIndex: Integer): TProvider;
function GetProviderIndex(AProviderName: String): Integer;
procedure ListProviders(AList: TStrings);
function NumProviders: Integer;
procedure RegisterProvider(AProvider: TProvider);
procedure SetProviderApiKey(const AProviderName, ApiKey: String);
procedure FreeProviders;


implementation

uses
  Contnrs, fpjson;

var
  Providers: TObjectList = nil;

{-------------------------------------------------------------------------------
                                TProvider
-------------------------------------------------------------------------------}
constructor TProvider.Create;
begin
  inherited Create;
end;

{ Default data handler: AList already contains data in the required way:
  - header line (ignored, usually "Date,Open,High,Low,Close,(...,)Volume")
  - comma-separated list of
    - date, formatted as specified by CSVDateFormat and CSVDateSeparator of provider
    - open
    - high
    - low
    - close
    - (more columns), ignored
    - volume, must be the last column
}
function TProvider.PrepareData(AList: TStrings): Boolean;
begin
  Result := AList <> nil;
end;


{-------------------------------------------------------------------------------
                       TAlphaVantageProvider
--------------------------------------------------------------------------------
  Documentation:
  Sample URL:
    https://www.alphavantage.co/query?function=TIME_SERIES_WEEKLY&symbol=MSFT&apikey=demo&datatype=csv
-------------------------------------------------------------------------------}
constructor TAlphaVantageProvider.Create;
begin
  inherited;
  Name := 'AlphaVantage';
  URL := 'https://www.alphavantage.co/query?'+
    'function=%4:s&' +             // resolution
    'datatype=csv&' +              // csv or json
    'symbol=%0:s&' +               // ticker symbol
    'apikey=%1:s';                 // api key
  AllowResolution := true;
  Resolutions[rDaily] := ''; //TIME_SERIES_DAILY';
  Resolutions[rWeekly] := 'TIME_SERIES_WEEKLY';
  Resolutions[rMonthly] := 'TIME_SERIES_MONTHLY';
  AllowDateRange := false;
  URLDateFormat := '';  // Not used
  CSVDateFormat := 'yyyy/mm/dd';
  CSVDateSeparator := '-';
  NeedsAPIKey := true;
  APIKey := '';  // will be queried by a dialog box and stored in ini file
end;

{-------------------------------------------------------------------------------
                       TMarketStackProvider
--------------------------------------------------------------------------------
 Documentation: https://marketstack.com/documentation
 Sample URL:
   http://api.marketstack.com/v1/eod?access_key=MY_API_KEY&symbols=AAPL&date_from=2023-06-26&date_to=2023-07-06
}
constructor TMarketStackProvider.Create;
begin
  inherited;
  Name := 'MarketStack';
  URL := 'http://api.marketstack.com/v1/eod?'+
    'access_key=%1:s&' +           // api key
    'symbols=%0:s&' +              // ticker symbol
    'limit=1000&' +
    'date_from=%2:s&' +            // start date in yyyy-mm-dd format
    'data_to=%3:s';                // end date in yyyy-mm-dd format
  AllowResolution := false;
  AllowDateRange := true; //false;
  URLDateFormat := 'yyyy-mm-dd';
  CSVDateFormat := 'yyyy/mm/dd';
  CSVDateSeparator := '-';
  NeedsAPIKey := true;
  APIKey := '';  // will be queried by a dialog box and stored in ini file
end;

(* Sample from MarketStack stream:

{"pagination":
   {"limit":100,
    "offset":0,
    "count":100,
    "total":210
   },
 "data":[
   {"open":120.06,
    "high":123.37,
    "low":120.06,
    "close":122.63,
    "volume":17830347.0,
    "adj_high":123.37,
    "adj_low":120.06,
    "adj_close":122.63,
    "adj_open":120.06,
    "adj_volume":17830347.0,
    "split_factor":1.0,
    "dividend":0.0,
    "symbol":"GOOG",
    "exchange":"XNAS",
    "date":"2023-07-05T00:00:00+0000"
   },
   {"open":120.32,
    ...
*)
function TMarketStackProvider.PrepareData(AList: TStrings): Boolean;
var
  json: TJSONData;
  jData: TJSONData;
  jDataArray: TJSONArray;
  jDataObj: TJSONObject;
  jDate: TJSONData;
  jOpen: TJSONData;
  jHigh: TJSONData;
  jLow: TJSONData;
  jClose: TJSONData;
  jVolume: TJSONData;
  d: TDateTime;
  lDate: String;
  lOpen: String;
  lHigh: String;
  lLow: String;
  lClose: String;
  lVolume: String;
  item: String;
  fmt: TFormatSettings;
  i: Integer;
begin
  Result := inherited;
  if not Result then
    exit;

  fmt := DefaultFormatSettings;
  fmt.DecimalSeparator := '.';
  fmt.DateSeparator := '-';
  fmt.ShortDateFormat := 'yyyy/mm/dd';

  json := GetJSON(AList.Text);
  try
    jData := json.FindPath('error');
    if jData is TJSONObject then begin
      jDataObj := TJSONObject(jData);
      jData := jDataObj.FindPath('message');
      AList.Clear;
      AList.Add(jData.AsString);
      Result := false;
      exit;
    end;

    jData := json.FindPath('data');
    if (jData is TJSONArray) then
    begin
      jDataArray := TJSONArray(jData);

      AList.Clear;
      AList.Add('Date,Open,High,Low,Close,Adj Close,Volume');
      for i := 0 to jDataArray.Count-1 do
        if jDataArray.Types[i] = jtObject then
        begin
          jDataObj := TJSONObject(jDataArray[i]);
          jDate := jDataObj.Find('date');
          jOpen := jDataObj.Find('open');
          jHigh := jDataObj.Find('high');
          jLow := jDataObj.find('low');
          jClose := jDataObj.Find('close');
          jVolume := jDataObj.Find('volume');
          if (jDate = nil) or not TryStrToDate(copy(jDate.AsString, 1, 10), d, fmt) then
            Continue;
          lDate := FormatDateTime('yyyy-mm-dd', d);
          if jOpen <> nil then lOpen := jOpen.AsString else lOpen := '';
          if jHigh <> nil then lHigh := jHigh.AsString else lHigh := '';
          if jLow <> nil then lLow := jLow.AsString else lLow := '';
          if jClose <> nil then lClose := jClose.AsString else lClose := '';
          if jVolume <> nil then lVolume := jVolume.AsString else lVolume := '';
          item := Format('%s,%s,%s,%s,%s,%s', [lDate, lOpen, lHigh, lLow, lClose, lVolume]);
          AList.Add(item);
        end;
    end;
  finally
    json.Free;
  end;
end;

{-------------------------------------------------------------------------------
                           TYahooFinanceProvider
--------------------------------------------------------------------------------
  based on
    https://www.mathworks.com/matlabcentral/fileexchange/37502-historical-stock-data-download-alternate-method

  Sample url (works, July 06 2023) --> downloads a csv file.
    https://query1.finance.yahoo.com/v7/finance/download/AAPL?period1=631152000&period2=1606521600&interval=1d&events=history&includeAdjustedClose=true

  Code from above MathWorks site, not working, but shows the essentials:
    site = strcat(
      'https://finance.yahoo.com/quote/',ticker,'/history?',...
      'period1=',d1u,'&period2=',d2u,'&interval=1',freq,'&filter=history&',...
      'frequency=1',freq);
    where
      ticker -- Yahoo ticker symbol for desired security.
      d1u -- start date for data. UNIX date time!. Default: 100 days ago
      d2u -- end date for data. UNIX date/time!. Default: today
      freq -- data frequency 'd' (daily), 'w' (weekly), or 'm' (monthly). Default = 'd'
-------------------------------------------------------------------------------}
constructor TYahooFinanceProvider.Create;
begin
  inherited;
  Name := 'Yahoo Finance';
  URL := 'https://query1.finance.yahoo.com/v7/finance/download/'+
    '%0:s?'+                        // ticker
    'period1=%2:s'+                 // start date, unix date/time
    '&period2=%3:s'+                // end date, unix date/time
    '&interval=1%4:s'+              // 'd' (daily), 'wk' (weekly), 'mo' monthly
    '&events=history'+
    '&includeAdjustedClose=true';
  Resolutions[rDaily] := 'd';
  Resolutions[rWeekly] := 'wk';
  Resolutions[rMonthly] := 'mo';
  AllowResolution := true;
  AllowDateRange := true;
  URLDateFormat := 'unix';  // unix datetime stamp = number of seconds since Jan 1, 1970
  CSVDateFormat := 'yyyy/mm/dd';
  CSVDateSeparator := '-';
  NeedsAPIKey := false;
  APIKey := '';  // not needed;
end;


{-------------------------------------------------------------------------------
                       Provider registration & related
-------------------------------------------------------------------------------}

{ CreateProviders }

procedure CreateProviders;
begin
  Providers := TObjectList.Create;

  // AlphaVantage
  RegisterProvider(TAlphaVantageProvider.Create);

  // MarketStack
  RegisterProvider(TMarketStackProvider.Create);

  // Yahoo finance
  RegisterProvider(TYahooFinanceProvider.Create);

  // Yahoo finance (old)
  // -------------
  // Parameters:
  // s = stock ticker symbol
  // a = start month (zero-based), b = start day, c = start year
  // d = end month (zero-based), e: end day, f: end year
  // g = interval ("w" = week, "d" = day)
  // Example:
  //   http://ichart.finance.yahoo.com/table.csv?s=GOOG&a=00&b=1&c=2000&d=09&e=7&f=2014&g=d&ignore=.csv
  //
  // NO LONGER AVAILABLE !!!
  (*
  P := TProvider.Create;
  P.ID := Providers.Count;
  P.Name := 'Yahoo Finance (old)';
  P.AllowResolution := true;
  P.Resolutions := NO_RESOLUTIONS;
  P.AllowDateRange := true;
  P.CSVDateFormat := 'yyyy/mm/dd';
  P.CSVDateSeparator := '-';
  P.NeedsAPIKey := false;
  P.APIKey := '';  // not needed
  Providers.Add(P);

  // Google
  //--------
  // Parameters:
  // q = ticker symbol of the stock (symbol lookup: http://www.google.com/finance)
  // startdate in format like "Nov 1, 2011" (without quotes)
  // enddate, dto.
  // Example:
  //   http://www.google.com/finance/historical?q=AAPL&startdate=Nov 1, 2011&enddate=Nov 30, 2011&output=csv
  //
  // NO LONGER AVAILABLE !!!
  P := TProvider.Create;
  P.ID := Providers.Count;
  P.Name := 'Google';
  P.AllowResolution := false;
  P.Resolutions := NO_RESOLUTIONS;
  P.AllowDateRange := true;
  P.CSVDateFormat := 'd/mmm/yy';
  P.CSVDateSeparator := '-';
  P.NeedsAPIKey := false;
  P.APIKey := '';  // not needed.
  Providers.Add(P);

  // Google
  // ------
  // Parameters:
  // q = ticker symbol of the stock (symbol lookup: http://www.google.com/finance)
  // startdate in format like "Nov 1, 2011" (without quotes)
  // enddate, dto.
  // Example:
  // http://www.google.com/finance/historical?q=AAPL&startdate=Nov 1, 2011&enddate=Nov 30, 2011&output=csv
  //
  // NO LONGER AVAILABLE !!!
  P := TProvider.Create;
  P.ID := Providers.Count;
  P.Name := 'Google';
  P.URL := 'http://www.google.com/finance/historical?'+
    'q=%0:s&startdate=%2:s&enddate=%3:s&output=csv';
  P.AllowDateRange := true;
  P.Resolutions := NO_RESOLUTIONS;
  P.URLDateFormat := 'mmm d", "yyyy';
  P.CSVDateFormat := 'd/mmm/yy';
  P.NeedsAPIKey := false;
  P.APIKey := '';  // not needed.
  Providers.Add(P);
  *)
      (*
  // Yahoo
  // --------
  // Parameters:
  // s = stock ticker symbol
  // a = start month (zero-based), b = start day, c = start year
  // d = end month (zero-based), e: end day, f: end year
  // g = interval ("w" = week, "d" = day)
  // Example:
  // http://ichart.finance.yahoo.com/table.csv?s=GOOG&a=00&b=1&c=2000&d=09&e=7&f=2014&g=d&ignore=.csv

  P := TProvider.Create;
  P.Name := 'Yahoo Finance';
  P.URL := 'https://ichart.yahoo.com/table.csv?s=%0:s&a=%d&b=%d&c=%d&d=%d&e=%d&f=%d&g=%s&ignore=.csv';
  P.AllowsDateRange := true;
  P.Resolutions := NO_RESOLUTIONS;
  P.URLDateFormat := '
  P.CSVDateFormat := 'yyyy/mm/dd';
  P.NeedsAPIKey := false;
  P.APIKey := '';  // not needed
  Providers.Add(P);
  *)
end;

function GetProvider(AIndex: Integer): TProvider;
begin
  Result := TProvider(Providers.Items[AIndex]);
end;

function GetProviderIndex(AProviderName: String): Integer;
begin
  for Result := 0 to Providers.Count-1 do
    if SameText(GetProvider(Result).Name, AProviderName) then
      exit;
  Result := -1;
end;

procedure ListProviders(AList: TStrings);
var
  i: Integer;
begin
  AList.Clear;
  for i:=0 to Providers.Count-1 do
    AList.Add(GetProvider(i).Name);
end;

function NumProviders: Integer;
begin
  Result := Providers.Count;
end;

procedure RegisterProvider(AProvider: TProvider);
begin
  AProvider.ID := Providers.Count;
  Providers.Add(AProvider);
end;

procedure SetProviderAPIKey(const AProviderName, APIKey: String);
var
  i: Integer;
  P: TProvider;
begin
  for i:=0 to Providers.Count-1 do
  begin
    P := GetProvider(i);
    if P.Name = AProviderName then
    begin
      P.ApiKey := APIKey;
      break;
    end;
  end;
end;

procedure FreeProviders;
begin
  Providers.Free;
end;

end.

