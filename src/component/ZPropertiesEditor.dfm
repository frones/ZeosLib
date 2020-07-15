object frmPropertyEditor: TfrmPropertyEditor
  Left = 352
  Top = 176
  Width = 637
  Height = 561
  Caption = 'Edit Properties ...'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    621
    522)
  PixelsPerInch = 96
  TextHeight = 13
  object pcEditValues: TPageControl
    Left = 0
    Top = 0
    Width = 621
    Height = 487
    ActivePage = tcDiffList
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    OnChanging = pcEditValuesChanging
    object tcDiffList: TTabSheet
      Caption = 'Diff-List'
      object Splitter1: TSplitter
        Left = 0
        Top = 278
        Width = 613
        Height = 5
        Cursor = crVSplit
        Align = alBottom
      end
      object pnlProps: TPanel
        Left = 0
        Top = 0
        Width = 613
        Height = 278
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 8
        TabOrder = 0
        object spltProps: TSplitter
          Left = 305
          Top = 8
          Width = 4
          Height = 262
        end
        object bgPropsUsed: TGroupBox
          Left = 8
          Top = 8
          Width = 297
          Height = 262
          Align = alLeft
          Caption = 'Used properties'
          TabOrder = 0
          DesignSize = (
            297
            262)
          object lbUsed: TListBox
            Left = 8
            Top = 16
            Width = 249
            Height = 238
            Anchors = [akLeft, akTop, akRight, akBottom]
            ItemHeight = 13
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            OnClick = lbUsedClick
            OnMouseMove = lbUsedMouseMove
          end
          object btnAdd: TButton
            Left = 264
            Top = 16
            Width = 25
            Height = 25
            Anchors = [akTop, akRight]
            Caption = '<'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 1
            OnClick = btnAddClick
          end
          object btnRemove: TButton
            Left = 264
            Top = 48
            Width = 25
            Height = 25
            Anchors = [akTop, akRight]
            Caption = '>'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 2
            OnClick = btnRemoveClick
          end
        end
        object gbAvailable: TGroupBox
          Left = 309
          Top = 8
          Width = 296
          Height = 262
          Align = alClient
          Caption = 'Available properties'
          TabOrder = 1
          DesignSize = (
            296
            262)
          object lbAvailable: TListBox
            Left = 8
            Top = 15
            Width = 281
            Height = 238
            Anchors = [akLeft, akTop, akRight, akBottom]
            ItemHeight = 13
            TabOrder = 0
            OnClick = lbAvailableClick
            OnMouseMove = lbAvailableMouseMove
          end
        end
      end
      object pnlBottom: TPanel
        Left = 0
        Top = 283
        Width = 613
        Height = 176
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        DesignSize = (
          613
          176)
        object lblProtocol: TLabel
          Left = 8
          Top = 144
          Width = 42
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'Protocol:'
        end
        object lblServerProvider: TLabel
          Left = 168
          Top = 144
          Width = 73
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'ServerProvider:'
          Visible = False
        end
        object lblHostversion: TLabel
          Left = 424
          Top = 144
          Width = 68
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'Hostversion: 0'
          Visible = False
        end
        object lblClientVersion: TLabel
          Left = 512
          Top = 144
          Width = 73
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'ClientVersion: 0'
          Visible = False
        end
        object cbProtocol: TComboBox
          Left = 56
          Top = 144
          Width = 105
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          OnChange = cbProtocolChange
        end
        object pnlValDesc: TPanel
          Left = 0
          Top = 0
          Width = 613
          Height = 137
          Align = alTop
          BevelOuter = bvNone
          BorderWidth = 8
          TabOrder = 1
          object Splitter2: TSplitter
            Left = 305
            Top = 8
            Height = 121
          end
          object gbVal: TGroupBox
            Left = 8
            Top = 8
            Width = 297
            Height = 121
            Align = alLeft
            Caption = 'Value'
            TabOrder = 0
            DesignSize = (
              297
              121)
            object lblProtocols: TLabel
              Left = 8
              Top = 40
              Width = 44
              Height = 13
              Caption = 'Protocols'
              Visible = False
              WordWrap = True
            end
            object lblProviders: TLabel
              Left = 144
              Top = 40
              Width = 44
              Height = 13
              Anchors = [akTop, akRight]
              Caption = 'Providers'
              Visible = False
              WordWrap = True
            end
            object cbEnum: TComboBox
              Left = 8
              Top = 88
              Width = 281
              Height = 21
              Style = csDropDownList
              Anchors = [akLeft, akTop, akRight]
              ItemHeight = 13
              TabOrder = 0
            end
            object edString: TEdit
              Left = 8
              Top = 16
              Width = 281
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 1
            end
          end
          object gbDescription: TGroupBox
            Left = 308
            Top = 8
            Width = 297
            Height = 121
            Align = alClient
            Caption = 'Purpose/Description'
            TabOrder = 1
            DesignSize = (
              297
              121)
            object mmDescrption: TMemo
              Left = 8
              Top = 16
              Width = 281
              Height = 97
              Anchors = [akLeft, akTop, akRight, akBottom]
              ReadOnly = True
              ScrollBars = ssVertical
              TabOrder = 0
            end
          end
        end
        object cbProvider: TComboBox
          Left = 248
          Top = 144
          Width = 113
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 2
        end
      end
    end
    object tsStringList: TTabSheet
      Caption = 'String-List'
      ImageIndex = 1
      DesignSize = (
        613
        459)
      object mmStringList: TMemo
        Left = 8
        Top = 8
        Width = 581
        Height = 443
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 0
      end
    end
  end
  object btnOK: TButton
    Left = 457
    Top = 491
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 537
    Top = 491
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object cbHideAlias: TCheckBox
    Left = 16
    Top = 496
    Width = 201
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Hide properties with equal purpose'
    TabOrder = 3
    OnClick = cbHideAliasClick
  end
end
