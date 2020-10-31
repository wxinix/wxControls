unit wxObjectInspector.EditDialog.Strings;

interface

uses
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  wxObjectInspector.Core;

type
  TzStringsEditDialog = class(TwxObjectInspectorDialog)
    BtnCancel: TButton;
    BtnOk: TButton;
    GroupBox: TGroupBox;
    LabelLines: TLabel;
    Memo: TMemo;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MemoChange(Sender: TObject);
  private
    FItems: TStrings;
    FSaveItems: TStringList;
  protected
    procedure Setup; override;
    procedure UpdateCount;
  end;

implementation

uses
  System.SysUtils;

{$R *.dfm}

procedure TzStringsEditDialog.BtnCancelClick(Sender: TObject);
begin
  FItems.Assign(FSaveItems);
  ModalResult := mrNone;
  Close;
end;

procedure TzStringsEditDialog.BtnOkClick(Sender: TObject);
begin
  FItems.Assign(Memo.Lines);
  ModalResult := mrOk;
  Close;
end;

procedure TzStringsEditDialog.FormCreate(Sender: TObject);
begin
  Memo.Clear;
  UpdateCount;
end;

procedure TzStringsEditDialog.FormDestroy(Sender: TObject);
begin
  FSaveItems.Free;
end;

procedure TzStringsEditDialog.MemoChange(Sender: TObject);
begin
  UpdateCount;
end;

procedure TzStringsEditDialog.Setup;
begin
  FItems := TStrings(PropertyItem.Value.AsObject);
  FSaveItems := TStringList.Create;
  FSaveItems.Assign(FItems);
  Memo.Lines.Assign(FItems);
end;

procedure TzStringsEditDialog.UpdateCount;
begin
  LabelLines.Caption := Format('%d line ', [Memo.Lines.Count])
end;

end.
