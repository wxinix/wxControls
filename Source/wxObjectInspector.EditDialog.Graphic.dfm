object zGraphicEditDialog: TzGraphicEditDialog
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Graphic Editor'
  ClientHeight = 286
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object BtnSave: TButton
    Left = 272
    Top = 253
    Width = 75
    Height = 25
    Caption = 'Save'
    ModalResult = 1
    TabOrder = 0
    OnClick = BtnSaveClick
  end
  object BtnCancel: TButton
    Left = 176
    Top = 253
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = BtnCancelClick
  end
  object BtnLoad: TButton
    Left = 47
    Top = 253
    Width = 99
    Height = 25
    Caption = 'Load...'
    TabOrder = 2
    OnClick = BtnLoadClick
  end
  object Panel: TPanel
    Left = 2
    Top = 6
    Width = 370
    Height = 241
    BevelInner = bvLowered
    ShowCaption = False
    TabOrder = 3
    object Image: TImage
      Left = 9
      Top = 8
      Width = 351
      Height = 225
      Center = True
    end
  end
  object OpenDialog: TOpenDialog
    Left = 154
    Top = 62
  end
end
