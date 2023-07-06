unit fcStockItems;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, fgl;

type
  TStockTickerSymbolList = specialize TFPGMap<string, string>;

  TStockItem = class
  private
    FCompany: String;
    FCurrency: String;
    FDefaultTickerSymbol: String;
    FTickerSymbols: TStockTickerSymbolList;
    function GetTickerSymbol(AProviderName: String): String;
  protected
  public
    constructor Create(const ACompany, ATickerSymbols, ACurrency: String);
    destructor Destroy; override;
    procedure ReadTickerSymbols(const ATickerSymbols: String);
    function WriteTickerSymbols: String;
    property Company: String read FCompany;
    property Currency: String read FCurrency;
    property TickerSymbol[AProviderName: String]: String read GetTickerSymbol;
  end;

function RegisterStockItem(ACompany: String; const ATickerSymbols: String;
  ACurrency: String): TStockItem;
function FindStockItem(ACompany: String): TStockItem;
function GetStockItem(AIndex: Integer): TStockItem;
procedure ClearStockItems;
procedure DeleteStockItem(ACompany: String);
procedure ListStockItemCompanies(AList: TStrings);
function NumStockItems: Integer;

implementation

var
  StockItems: TObjectList = nil;

function CompareStockItems(AItem1, AItem2: Pointer): Integer;
begin
  Result := CompareStr(TStockItem(AItem1).Company, TStockItem(AItem2).Company);
end;

procedure ClearStockItems;
begin
  StockItems.Clear;
end;

procedure DeleteStockItem(ACompany: String);
var
  item: TStockItem;
  idx: Integer;
begin
  item := FindStockItem(ACompany);
  if item <> nil then
  begin
    idx := StockItems.IndexOf(item);
    StockItems.Delete(idx);
  end;
end;

function FindStockItem(ACompany: String): TStockItem;
var
  i: Integer;
begin
  for i := 0 to StockItems.Count-1 do
  begin
    Result := TStockItem(StockItems[i]);
    if SameText(Result.Company, ACompany) then
      exit;
  end;
  Result := nil;
end;

function GetStockItem(AIndex: Integer): TStockItem;
begin
  Result := TStockItem(StockItems[AIndex]);
end;

procedure ListStockItemCompanies(AList: TStrings);
var
  i: Integer;
begin
  AList.BeginUpdate;
  try
    AList.Clear;
    for i := 0 to StockItems.Count-1 do
      AList.Add(TStockItem(StockItems[i]).Company);
  finally
    AList.EndUpdate;
  end;
end;

function NumStockItems: Integer;
begin
  Result := StockItems.Count;
end;

function RegisterStockItem(ACompany: String; const ATickerSymbols: String;
  ACurrency: String): TStockItem;
begin
  Result := FindStockItem(ACompany);
  if Result = nil then
  begin
    // Company not found: add a new item
    Result := TStockItem.Create(ACompany, ATickerSymbols, ACurrency);
    StockItems.Add(Result);
    StockItems.Sort(@CompareStockItems);
  end;
end;


{ TStockItem }

constructor TStockItem.Create(const ACompany, ATickerSymbols, ACurrency: String);
begin
  inherited Create;
  FTickerSymbols := TStockTickerSymbolList.Create;
  FTickerSymbols.Sorted := true;
  FCompany := ACompany;
  FCurrency := ACurrency;
  ReadTickerSymbols(ATickerSymbols);
end;

destructor TStockItem.Destroy;
begin
  FTickerSymbols.Free;
  inherited;
end;

{ Returns the ticker symbol used by the specified registered provider }
function TStockItem.GetTickerSymbol(AProviderName: String): String;
var
  idx: Integer;
begin
  if FTickerSymbols.Find(AProviderName, idx) then
    Result := FTickerSymbols.Data[idx]
  else
    Result := FDefaultTickerSymbol;
end;

// ATickerSymbols is a '|' separated list of ticker symbol names and providers:
//  1st item: default ticker symbol
//  2nd item: provider name for the ticker symbol given as 3rd item.
//            (provider name as registered in fcProviders).
//  etc.
// Example: 'MSFT|Microsoft|NASDAQ:MSFT'
procedure TStockItem.ReadTickerSymbols(const ATickerSymbols: String);
var
  sa: TStringArray;
  i: Integer;
begin
  sa := ATickerSymbols.Split('|');
  if not odd(Length(sa)) then
    raise Exception.Create('[TStockItem.ReadTickerSymbols] Invalid structure of Tickersymbol list.');
  FDefaultTickerSymbol := sa[0];
  i := 1;
  while i < Length(sa) do
  begin
    FTickerSymbols.Add(sa[i], sa[i+1]);
    inc(i, 2);
  end;
end;

{ Stuffs the ticker symbols along with the name of the provider using this
  symbols into a single string. Items are separated by '|'.
  See also ReadTickerSymbols. }
function TStockItem.WriteTickerSymbols: String;
var
  i: Integer;
begin
  Result := FDefaultTickerSymbol;
  for i := 0 to FTickerSymbols.Count-1 do
    Result := Format('%s|%s|%s', [Result, FTickerSymbols.Keys[i], FTickerSymbols.Data[i]]);
end;

initialization
  StockItems := TObjectList.Create;

  RegisterStockItem('Apple Inc', 'AAPL|Google|NASDAQ:AAPL', 'USD');
  RegisterStockItem('CISCO', 'CSCO|Google|NASDAQ:CSCO', 'USD');
  RegisterStockItem('Facebook Inc', 'FB|Google|NASDAQ:FB', 'USD');
  RegisterStockItem('Google Inc', 'GOOG|Google|NASDAQ:GOOG', 'USD');
  RegisterStockItem('IBM Inc', 'IBM|Google|NYSE:IBM', 'USD');
  RegisterStockItem('Infineon (Frankfurt)', 'IFX.DE|MarketStack|IFX.XFRA|Google|IFX.FRA', 'EUR');
  RegisterStockItem('Microsoft', 'MSFT|Google|NASDAQ:MSFT', 'USD');
  RegisterStockItem('Motorola Solutions Inc', 'MSI|Google|NASDAQ:MSI', 'USD');
  RegisterStockItem('Siemens', 'SIE.DE|Google|FRA:SIE|MarketStack|SIE.XFRA', 'EUR');
  RegisterStockItem('Volkswagen', 'VWAGY', 'EUR');

finalization
  StockItems.Free;

end.

