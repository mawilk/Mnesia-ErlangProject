Mnesia-ErlangProject
====================

Prezentacja dotycząca DBMS Mnesia

Przygotowali: Mateusz Kantor, Magdalena Wilk

### Załączone pliki
Mnesia.docx - opis tematu

prezentacja.pptx - slajdy wykorzystane na prezentacji tematu podczas zajęć

start.erl, input.erl - program wykorzystany jako przykład 

### Uruchomienie przykładu

Naszą baze powinniśmy umieścić w jakimś wybranym folderze.
```
cd("C:\\FolderNaBaze").
```
Utworzenie schematu przykładowej bazy:
```
c(start).
start:install([node()]).
```
Przykładowe dane znajdują się w pliku input.erl.
```
c(input).
input:add_chars().
input:add_enemies1().
input:add_enemies2().
```

Dostępne funkcje:
```
start:kill_enemy/2    %przyklad usuwania rekordu oraz modyfikacji pola innego rekordu
start:find_by_spec/1  %przyklad wyszukania rekordu
start:add_enemy/3     %przyklad dodania rekordu
start:add_part/1
start:add_character/3
start:add_to_party/2
```
