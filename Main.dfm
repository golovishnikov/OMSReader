object MainForm: TMainForm
  Left = 319
  Top = 197
  Width = 650
  Height = 936
  Caption = #1063#1090#1077#1085#1080#1077' '#1082#1072#1088#1090' '#1054#1052#1057
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 650
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 458
    Width = 634
    Height = 440
    Align = alClient
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object OwnerInfo: TValueListEditor
    Left = 0
    Top = 17
    Width = 634
    Height = 424
    Align = alTop
    Strings.Strings = (
      #1053#1086#1084#1077#1088' '#1087#1086#1083#1080#1089#1072'=OI.MedicalIncuranceNumber'
      #1044#1072#1090#1072' '#1089#1086#1079#1076#1072#1085#1080#1103'=Date2Str(OI.RegistrationDate)'
      #1044#1077#1081#1089#1090#1074#1091#1077#1090' '#1076#1086'=OI.MedicalInsuranceDateEnd'
      #1053#1086#1084#1077#1088' '#1082#1072#1088#1090#1086#1095#1082#1080'?=OI.Policy'
      #1060#1072#1084#1080#1083#1080#1103'=OI.LName'
      #1048#1084#1103'=OI.FName'
      #1054#1090#1095#1077#1089#1090#1074#1086'=OI.MName'
      #1055#1086#1083'=OI.SexName'
      #1044#1072#1090#1072' '#1088#1086#1078#1076#1077#1085#1080#1103'=OI.Byrthday'
      #1052#1077#1089#1090#1086' '#1088#1086#1078#1076#1077#1085#1080#1103'=OI.BirthPlace'
      #1050#1086#1076' '#1089#1090#1088#1072#1085#1099'=OI.CountryCode'
      #1053#1072#1079#1074#1072#1085#1080#1077' '#1089#1090#1088#1072#1085#1099'=OI.Country'
      #1057#1053#1048#1051#1057'=OI.InsuranceCertificate'
      #1060#1086#1088#1084#1072#1090' '#1080#1079#1086#1073#1088#1072#1078#1077#1085#1080#1103'=OI.ImgFormat'
      #1044#1072#1085#1085#1099#1077' '#1092#1086#1090#1086#1088#1075#1072#1092#1080#1080', '#1089#1086#1086#1090#1074#1077#1090#1089#1090#1074#1091#1102#1097#1080#1077' ImgFormat=OI.Img'
      #1054#1043#1056#1053' '#1076#1083#1103' '#1057#1052#1054'=OI.OGRN'
      #1054#1050#1040#1058#1054' '#1076#1083#1103' '#1057#1052#1054'=OI.OKATO'
      #1044#1072#1090#1072' '#1085#1072#1095#1072#1083#1072' '#1089#1090#1088#1072#1093#1086#1074#1072#1085#1080#1103' '#1074' '#1057#1052#1054'=OI.DateBegin'
      #1044#1072#1090#1072' '#1086#1082#1086#1085#1095#1072#1085#1080#1103' '#1089#1090#1088#1072#1093#1086#1074#1072#1085#1080#1103' '#1074' '#1057#1052#1054'=OI.DateEnd'
      '')
    TabOrder = 1
    ColWidths = (
      150
      478)
  end
  object StaticText1: TStaticText
    Left = 0
    Top = 0
    Width = 634
    Height = 17
    Align = alTop
    Caption = #1044#1072#1085#1085#1099#1077' '#1087#1086#1083#1080#1089#1072
    TabOrder = 2
  end
  object StaticText2: TStaticText
    Left = 0
    Top = 441
    Width = 634
    Height = 17
    Align = alTop
    Caption = #1051#1086#1075' '#1074#1079#1072#1080#1084#1086#1076#1077#1081#1089#1090#1074#1080#1103' '#1089' '#1082#1072#1088#1090#1088#1080#1076#1077#1088#1086#1084
    TabOrder = 3
  end
  object XPManifest1: TXPManifest
    Left = 392
    Top = 65528
  end
end
