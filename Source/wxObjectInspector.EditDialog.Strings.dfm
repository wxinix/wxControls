object zStringsEditDialog: TzStringsEditDialog
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'String List Editor'
  ClientHeight = 354
  ClientWidth = 467
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    467
    354)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox: TGroupBox
    Left = 8
    Top = 8
    Width = 451
    Height = 307
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    DesignSize = (
      451
      307)
    object LabelLines: TLabel
      Left = 7
      Top = 13
      Width = 49
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      Caption = 'LabelLines'
    end
    object Memo: TMemo
      Left = 6
      Top = 31
      Width = 437
      Height = 272
      Align = alCustom
      Anchors = [akLeft, akTop, akRight, akBottom]
      Lines.Strings = (
        '')
      ScrollBars = ssBoth
      TabOrder = 0
      OnChange = MemoChange
    end
  end
  object BtnOk: TButton
    Left = 358
    Top = 321
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 1
    OnClick = BtnOkClick
  end
  object BtnCancel: TButton
    Left = 269
    Top = 321
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    OnClick = BtnCancelClick
  end
end
