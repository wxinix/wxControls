unit uMain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.ExtCtrls,
  Vcl.Styles,
  Vcl.Themes,
  Vcl.Grids,
  Vcl.ValEdit,
  Vcl.Menus,
  wxObjectInspector.Core;

type
  TMain = class(TForm)
    zObjectInspector1: TwxObjectInspector;
    Panel1: TPanel;
    Panel2: TPanel;
    GroupBox1: TGroupBox;
    Panel3: TPanel;
    LabeledEdit1: TLabeledEdit;
    SpeedButton1: TSpeedButton;
    CheckBox1: TCheckBox;
    RadioButton1: TRadioButton;
    Label1: TLabel;
    ObjsCombo: TComboBox;
    CheckBox2: TCheckBox;
    Label2: TLabel;
    StylesCombo: TComboBox;
    BtnMultiComponents: TButton;
    Memo1: TMemo;
    ListBox1: TListBox;
    SpeedButton2: TSpeedButton;
    Image1: TImage;
    BalloonHint1: TBalloonHint;
    Label3: TLabel;
    PopupMenu1: TPopupMenu;
    PopupItemTest1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure ObjsComboChange(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure StylesComboChange(Sender: TObject);
    procedure BtnMultiComponentsClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    function zObjectInspector1BeforeAddItem(Sender: TControl; AItem: PwxPropertyItem): Boolean;
  private
    FIncludeEvent: Boolean;
    procedure GetObjsList;
    procedure EnumStyles;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Main: TMain;

implementation

uses TypInfo;
{$R *.dfm}

procedure TMain.BtnMultiComponentsClick(Sender: TObject);
var
  Host: TwxObjectHost;
  i: Integer;
begin
  Host := TwxObjectHost.Create;
  with GroupBox1 do
    for i := 0 to ControlCount - 1 do
      Host.AddObject(Controls[i], Controls[i].Name);

  zObjectInspector1.Component := Host;
end;

procedure TMain.CheckBox2Click(Sender: TObject);
begin
  FIncludeEvent := TCheckBox(Sender).Checked;
  zObjectInspector1.UpdateProperties(True);
end;

procedure TMain.ObjsComboChange(Sender: TObject);
var
  Com: TComponent;
begin
  Com := nil;
  with TComboBox(Sender) do
  begin
    if ItemIndex > - 1 then
      Com := TComponent(Items.Objects[ItemIndex]);
  end;
  if Assigned(Com) then
    zObjectInspector1.Component := Com;
end;

procedure TMain.StylesComboChange(Sender: TObject);
begin
  with TComboBox(Sender) do
  begin
    if ItemIndex > - 1 then
      TStyleManager.SetStyle(Items[ItemIndex]);
  end;
end;

procedure TMain.EnumStyles;
var
  s: string;
begin
  StylesCombo.Clear;
  for s in TStyleManager.StyleNames do
    StylesCombo.Items.Add(s);
end;

procedure TMain.GetObjsList;
var
  i: Integer;
begin
  ObjsCombo.Clear;
  ObjsCombo.Text := '';
  with GroupBox1 do
    for i := 0 to ControlCount - 1 do
      ObjsCombo.Items.AddObject(Controls[i].Name, Controls[i]);
  ObjsCombo.Items.AddObject(PopupItemTest1.Name, PopupItemTest1);
  with ObjsCombo do
  begin
    ItemIndex := 0;
    zObjectInspector1.Component := Items.Objects[ItemIndex];
  end;

  ObjsCombo.Items.AddObject('MainForm', Self);
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  GetObjsList;
  EnumStyles;
end;

procedure TMain.FormResize(Sender: TObject);
begin
  Self.zObjectInspector1.Invalidate;
end;

function TMain.zObjectInspector1BeforeAddItem(Sender: TControl; AItem: PwxPropertyItem): Boolean;
begin
  Result := True;
  if not FIncludeEvent then
    Result := AItem.AssociatedProperty.PropertyType.TypeKind <> tkMethod;
end;

end.
