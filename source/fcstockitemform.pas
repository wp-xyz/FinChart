unit fcStockItemForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  StdCtrls, Dialogs, ButtonPanel, ValEdit, Grids;

type

  { TStockItemForm }

  TStockItemForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    EdCompany: TEdit;
    edDefaultTicker: TEdit;
    EdCurrency: TEdit;
    LblCompanyName: TLabel;
    lblDefaultTicker: TLabel;
    LblSymbol1: TLabel;
    vleTickerSymbols: TValueListEditor;
    procedure OKButtonClick(Sender: TObject);
    procedure vleTickerSymbolsPrepareCanvas(Sender: TObject; aCol,
      aRow: Integer; aState: TGridDrawState);
  private
    { private declarations }
    function GetTickerSymbols: String;
    procedure SetTickerSymbols(const AValue: String);
    function ValidData(out AMsg: String; out AControl: TWinControl): Boolean;
  public
    { public declarations }
    property TickerSymbols: String read GetTickerSymbols write SetTickerSymbols;
  end;

var
  StockItemForm: TStockItemForm;

implementation

{$R *.lfm}

uses
  fcProviders, fcStockItems;

const
  DEFAULT_TEXT = '(default)';

{ TStockItemForm }

function TStockItemForm.GetTickerSymbols: String;
var
  i: Integer;
  sym: String;
begin
  Result := edDefaultTicker.Text;
  for i := 1 to vleTickerSymbols.RowCount-1 do
  begin
    sym := vleTickerSymbols.Cells[1, i];
    if (sym <> '') and (sym <> edDefaultTicker.Text) and (sym <> DEFAULT_TEXT) then
      Result := Result + '|' + vleTickerSymbols.Cells[0, i] + '|' + sym;
  end;
end;

procedure TStockItemForm.OKButtonClick(Sender: TObject);
var
  msg: String;
  C: TWinControl;
begin
  if not ValidData(msg, C) then ModalResult := mrNone;
end;

procedure TStockItemForm.vleTickerSymbolsPrepareCanvas(Sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
var
  txt: String;
begin
  if (ACol = 1) and (ARow > 0) then
  begin
    txt := vleTickerSymbols.Cells[ACol, ARow];
    if txt = DEFAULT_TEXT then
      vleTickerSymbols.Canvas.Font.Color := clSilver;
  end;
end;

procedure TStockItemForm.SetTickerSymbols(const AValue: String);
var
  sa: TStringArray;
  i, j: Integer;
  L: TStrings;
  p: TProvider;
  stockItem: TStockItem;
  sym: String;
begin
  stockItem := FindStockItem(edCompany.Text);

  sa := AValue.Split('|');
  if Length(sa) > 0 then
    edDefaultTicker.Text := sa[0]
  else
    edDefaultTicker.Clear;

  L := TStringList.Create;
  try
    i := 1;
    while i < Length(sa) do
    begin
      L.Add(sa[i] + '=' + sa[i+1]);
      inc(i, 2);
    end;

    vleTickerSymbols.RowCount := NumProviders + 1;
    for i := 0 to NumProviders-1 do
    begin
      p := GetProvider(i);
      sym := stockItem.TickerSymbol[p.Name];
      vleTickerSymbols.Cells[0, i+1] := p.Name;
      if (sym <> sa[0]) then
        vleTickerSymbols.Cells[1, i+1] := sym
      else
        vleTickerSymbols.Cells[1, i+1] := DEFAULT_TEXT;
    end;
  finally
    L.Free;
  end;
end;

function TStockItemForm.ValidData(out AMsg: String;
  out AControl: TWinControl): Boolean;
begin
  Result := false;
  AMsg := '';
  AControl := nil;

  if EdCompany.Text = '' then begin
    AMsg := 'This field cannot be empty.';
    AControl := EdCompany;
    exit;
  end;

  if edDefaultTicker.Text = '' then begin
    AMsg := 'This field cannot be empty.';
    AControl := edDefaultTicker;
    exit;
  end;
  (*
  if EdAlphaVantageTicker.Text = '' then begin
    AMsg := 'This field cannot be empty.';
    AControl := EdAlphaVantageTicker;
    exit;
  end;
  if EdMarketStackTicker.Text = '' then begin
    AMsg := 'This field cannot be empty.';
    AControl := EdMarketStackTicker;
    exit;
  end;
  *)

  Result := true;
end;

end.

