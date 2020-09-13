object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 646
  ClientWidth = 1007
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object tcGroup: TTabControl
    Left = 0
    Top = 29
    Width = 1007
    Height = 49
    Align = alTop
    TabOrder = 0
    OnChanging = tcGroupChanging
    ExplicitTop = 23
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 78
    Width = 1007
    Height = 152
    Align = alTop
    Caption = 'SQL'
    TabOrder = 1
    ExplicitTop = 25
    object Splitter1: TSplitter
      Left = 2
      Top = 147
      Width = 1003
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      ExplicitLeft = 18
      ExplicitTop = 158
    end
    object mmSQL: TMemo
      Left = 2
      Top = 15
      Width = 871
      Height = 132
      Align = alLeft
      TabOrder = 0
    end
    object btnOpen: TButton
      Left = 896
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Open'
      TabOrder = 1
      OnClick = btnOpenClick
    end
    object btnExecSQL: TButton
      Left = 896
      Top = 64
      Width = 75
      Height = 25
      Caption = 'Execute'
      TabOrder = 2
      OnClick = btnExecSQLClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 0
    Top = 230
    Width = 1007
    Height = 416
    Align = alClient
    Caption = 'Results'
    TabOrder = 2
    ExplicitLeft = 232
    ExplicitTop = 264
    ExplicitWidth = 185
    ExplicitHeight = 105
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 230
    Width = 1007
    Height = 416
    Align = alClient
    DataSource = DataSource1
    TabOrder = 3
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 1007
    Height = 29
    Caption = 'ToolBar1'
    TabOrder = 4
    ExplicitLeft = 8
    object tbConnect: TToolButton
      Left = 0
      Top = 0
      Caption = 'tbConnect'
      ImageIndex = 0
      OnClick = tbConnectClick
    end
  end
  object MainMenu1: TMainMenu
    Left = 496
    Top = 328
    object File1: TMenuItem
      Caption = '&Datei'
      object New1: TMenuItem
        Caption = '&Neu'
      end
      object Open1: TMenuItem
        Caption = #214'&ffnen...'
      end
      object Save1: TMenuItem
        Caption = '&Speichern'
      end
      object SaveAs1: TMenuItem
        Caption = 'Speichern &unter...'
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Print1: TMenuItem
        Caption = '&Drucken...'
      end
      object PrintSetup1: TMenuItem
        Caption = 'Drucker&einrichtung...'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = '&Beenden'
      end
    end
  end
  object ZConnection1: TZConnection
    ControlsCodePage = cCP_UTF16
    Port = 0
    Left = 168
    Top = 280
  end
  object ZQuery1: TZQuery
    Connection = ZConnection1
    Params = <>
    Left = 248
    Top = 280
  end
  object DataSource1: TDataSource
    DataSet = ZQuery1
    Left = 312
    Top = 288
  end
end
