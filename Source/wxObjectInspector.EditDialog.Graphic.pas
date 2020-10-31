unit wxObjectInspector.EditDialog.Graphic;

interface

uses
  System.Classes,
  System.SysUtils,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.StdCtrls,
  wxObjectInspector.Core;

type
  TzGraphicEditDialog = class(TwxObjectInspectorDialog)
    BtnCancel: TButton;
    BtnLoad: TButton;
    BtnSave: TButton;
    Image: TImage;
    OpenDialog: TOpenDialog;
    Panel: TPanel;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnLoadClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FFilter: string;
    FGraphic: TGraphic;
    FUserGraphic: TGraphic;
  protected
    procedure DisplayGraphic(AGraphic: TGraphic);
    procedure Setup; override;
  end;

implementation

{$R *.dfm}

procedure TzGraphicEditDialog.BtnCancelClick(Sender: TObject);
begin
  ModalResult := mrNone;
  Close;
end;

procedure TzGraphicEditDialog.BtnLoadClick(Sender: TObject);
begin
  OpenDialog.Filter := FFilter;

  if OpenDialog.Execute then
  begin
    FUserGraphic.LoadFromFile(OpenDialog.FileName);
    DisplayGraphic(FUserGraphic);
  end;
end;

procedure TzGraphicEditDialog.BtnSaveClick(Sender: TObject);
begin
  FGraphic.Assign(FUserGraphic);
  ModalResult := mrOk;
  Close;
end;

procedure TzGraphicEditDialog.DisplayGraphic(AGraphic: TGraphic);
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  bmp.Assign(AGraphic);
  Image.Picture.Bitmap := bmp;
  bmp.Free;
end;

procedure TzGraphicEditDialog.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FUserGraphic);
end;

procedure TzGraphicEditDialog.Setup;
const
  bmpFilter  = 'Bitmaps|*.bmp';
  iconFilter = 'Icons|*.ico';
begin
  FGraphic := TGraphic(PropertyItem.Value.AsObject);

  if FGraphic is TBitmap then
    FFilter := bmpFilter
  else
    if FGraphic is TIcon then
      FFilter := iconFilter
    else
      FFilter := EmptyStr;
  // We directly operate on FUserGraphic. FGraphic referens to the original
  // data in PropertyItem. So we don't touch it till save.
  FUserGraphic := TGraphicClass(FGraphic.ClassType).Create;
  FUserGraphic.Assign(FGraphic);

  if not FUserGraphic.Empty then
    DisplayGraphic(FUserGraphic);
end;

end.
