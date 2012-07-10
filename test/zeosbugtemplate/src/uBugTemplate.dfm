object frmBugTemplate: TfrmBugTemplate
  Left = 0
  Top = 0
  Caption = 'frmBugTemplate'
  ClientHeight = 353
  ClientWidth = 523
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    523
    353)
  PixelsPerInch = 96
  TextHeight = 13
  object memProgress: TMemo
    Left = 8
    Top = 32
    Width = 507
    Height = 282
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      
        'Please read the comments in the code of Runme button to help wit' +
        'h this template ')
    TabOrder = 0
  end
  object edtBugTitle: TEdit
    Left = 8
    Top = 8
    Width = 507
    Height = 21
    TabOrder = 1
    Text = 
      'Change the text property to say what bug or problem you are tryi' +
      'ng to report'
  end
  object btnRunMe: TButton
    Left = 8
    Top = 320
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Run Me'
    TabOrder = 2
    OnClick = btnRunMeClick
  end
  object btnClose: TButton
    Left = 440
    Top = 320
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    TabOrder = 3
    OnClick = btnCloseClick
  end
  object ZQuery1: TZQuery
    Params = <>
    Left = 360
    Top = 208
  end
end
