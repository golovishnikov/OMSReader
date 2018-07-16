# Пример чтения данных с электронной карты ОМС

Приложение собиралось под Delphi 7.

В приложении использован код взаимодействия с ридером из [Delphi sample code for communication with smartcards via PC/SC API](http://files.identiv.com/products/smart-card-readers/developer-tools/Delphi_PCSC_Sample_Application_V1.0.0.4.zip)
Идентификаторы полей данных взяты из [Приложение для чтения данных с карт ОМС](https://github.com/lr131/OMSReader/blob/master/omsread.py), написанного на python.

Приложение автоматически находит все картридеры и следит за их состоянием. 
Приложение реагирует на вставку/извлечение карты. При вставке карты читаются данные и отображаются в форме. Так же фиксируется лог взаимодействия с ридером.