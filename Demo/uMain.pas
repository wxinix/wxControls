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
  wxObjectInspector.Core,
  mORMOt,
  SynCommons,
  Rtti;

type

 /// <summary>
  ///   Provides the "static" segment attributes that don't change during run time,
  ///   such as coordinates, lengths, names, number of lanes.
  /// </summary>
  TSegmentAttributes = class(TSQLRecord)
  strict private
    {$REGION 'Fields'}
    FSID: Integer;
    FSLat: Double;
    FSLon: Double;
    FSName: RawUTF8;
    FEID: Integer;
    FELat: Double;
    FELon: Double;
    FEName: RawUTF8;
    FWKT: RawUTF8;
    FLength: Integer;
    FNum_Lanes: Integer;
    FTolling_Zone_ID: Integer;
    {$ENDREGION}
  published
    /// <summary>
    ///   ID of the start point.
    /// </summary>
    property SID: Integer read FSID write FSID;

    /// <summary>
    ///   Latitude of the start point.
    /// </summary>
    property SLat: Double read FSLat write FSLat;

    /// <summary>
    ///   Longitude of the start point.
    /// </summary>
    property SLon: Double read FSLon write FSLon;

    /// <summary>
    ///   Name of the start point.
    /// </summary>
    property SName: RawUTF8 read FSName write FSName;

    /// <summary>
    ///   ID of the end point.
    /// </summary>
    property EID: Integer read FEID write FEID;

    /// <summary>
    ///   Latitude of the end point.
    /// </summary>
    property ELat: Double read FELat write FELat;

    /// <summary>
    ///   Longitude of the end point.
    /// </summary>
    property ELon: Double read FELon write FELon;

    /// <summary>
    ///   Name of the end point.
    /// </summary>
    property EName: RawUTF8 read FEName write FEName;

    /// <summary>
    ///   WKT representation of the segment.
    /// </summary>
    property WKT: RawUTF8 read FWKT write FWKT;

    /// <summary>
    ///   Length of the segment in feet.
    /// </summary>
    property Length: Integer read FLength write FLength;

    /// <summary>
    ///   Number of lanes.
    /// </summary>
    property Num_Lanes: Integer read FNum_Lanes write FNum_Lanes;

    /// <summary>
    ///   ID of the tolling zone that this segment belongs to.
    /// </summary>
    property Tolling_Zone_ID: Integer read FTolling_Zone_ID write FTolling_Zone_ID;
  end;
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
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ObjsComboChange(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure StylesComboChange(Sender: TObject);
    procedure BtnMultiComponentsClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    function zObjectInspector1BeforeAddItem(Sender: TControl; AItem: PwxPropertyItem): Boolean;
    function zObjectInspector1ItemSetValue(Sender: TControl; AItem: PwxPropertyItem; var ANewValue: TValue): Boolean;
  private
    FData: TObject;
    FIncludeEvent: Boolean;
    procedure GetObjsList;
    procedure EnumStyles;

  public
    { Public declarations }
  end;

var
  Main: TMain;

implementation

uses
  TypInfo;
{$R *.dfm}

procedure TMain.FormDestroy(Sender: TObject);
begin
  FData.Free;
end;

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
  FData := TSegmentAttributes.Create;
  zObjectInspector1.Component := FData;
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

function TMain.zObjectInspector1ItemSetValue(Sender: TControl; AItem: PwxPropertyItem; var ANewValue: TValue): Boolean;
begin
  Result := not ANewValue.IsEmpty;
end;

end.
