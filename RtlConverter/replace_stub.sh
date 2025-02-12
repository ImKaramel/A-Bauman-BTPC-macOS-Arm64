#!/bin/bash

# Проверка наличия параметров
if [ $# -ne 2 ]; then
  echo "Использование: $0 <путь_к_входному_файлу> <путь_к_файлу_с_вставками>"
  exit 1
fi

# Чтение параметров командной строки
input_file="$1"
stub_file="$2"
temp_file="temp_file"

# Проверка существования исходного файла и файла с вставками
if [ ! -f "$input_file" ]; then
  echo "Ошибка: файл $input_file не найден!"
  exit 1
fi

if [ ! -f "$stub_file" ]; then
  echo "Ошибка: файл $stub_file не найден!"
  exit 1
fi

# Обработка файла с использованием awk
awk -v stub_file="$stub_file" '
  { 
    print $0; 
    if ($0 ~ /\$\$\$STUB\$\$\$/) { 
      while ((getline line < stub_file) > 0) 
        print line; 
      print ""; 
    }
  }
' "$input_file" > "$temp_file"

# Перемещение временного файла обратно в оригинальный файл
mv "$temp_file" "$input_file"

echo "Обработка завершена, файл обновлен."
