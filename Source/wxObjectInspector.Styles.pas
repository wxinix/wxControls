unit wxObjectInspector.Styles;

interface

uses
  Vcl.Controls,
  Vcl.Forms,
  WinApi.Windows,
  WinApi.Messages;

type
  TzObjectInspectorScrollingStyleHook = class(TScrollingStyleHook)
  private
    procedure WMVScroll(var AMessage: TMessage); message WM_VSCROLL;
  public
    constructor Create(AControl: TWinControl); override;
  end;

  TzObjectInspectorItemHintWindow = class(THintWindow)
  private
    FPaintBold: Boolean;
  protected
    procedure Paint; override;
  public
    function CalcHintRect(AMaxWidth: Integer; const AHint: string; AData: TCustomData): TRect; override;
  end;

implementation

uses
  System.UITypes, Vcl.Graphics;

constructor TzObjectInspectorScrollingStyleHook.Create(AControl: TWinControl);
begin
  inherited;
end;

procedure TzObjectInspectorScrollingStyleHook.WMVScroll(var AMessage: TMessage);
begin
  inherited;
end;

function TzObjectInspectorItemHintWindow.CalcHintRect(AMaxWidth: Integer; const AHint: string; AData: TCustomData): TRect;
var
  oldFontStyle: TFontStyles;
begin
  oldFontStyle := Canvas.Font.Style;
  FPaintBold := Boolean(AData);

  if FPaintBold then { Must set bold before inheriting! }
    Canvas.Font.Style := Canvas.Font.Style + [fsBold];

  Result := inherited CalcHintRect(AMaxWidth, AHint, AData);
  Canvas.Font.Style := oldFontStyle;
end;

procedure TzObjectInspectorItemHintWindow.Paint;
var
  oldFontStyle: TFontStyles;
begin
  oldFontStyle := Canvas.Font.Style;

  if FPaintBold then
    Canvas.Font.Style := Canvas.Font.Style + [fsBold];

  inherited;
  Canvas.Font.Style := oldFontStyle;
end;

end.
