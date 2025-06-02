````
# Функциональный калькулятор на F# (Linux)

## Оглавление
1. [Требования](#требования)  
2. [Установка .NET SDK](#установка-.net-sdk)  
3. [Создание и настройка проекта](#создание-и-настройка-проекта)  
4. [Добавление исходного файла](#добавление-исходного-файла)  
5. [Сборка и запуск](#сборка-и-запуск)  
6. [Примеры использования](#примеры-использования)  
---

## Требования

- **Операционная система:** любая современная Linux-система (Ubuntu, Debian, Fedora, Arch и т. д.).  
- **Процессор:** x86_64 или ARM64 с поддержкой .NET 9.  
- **Память:** минимум 100 МБ свободной оперативной памяти.  
- **Пользовательские привилегии:** умение устанавливать пакеты (через `apt`, `dnf`, `pacman` и т. п.) и пользоваться терминалом.  

---

## Установка .NET SDK

1. **Проверка текущей установки**  
   Откройте терминал и введите:
   ```bash
   dotnet --info
````

Если вы видите информацию о .NET 9.x (или выше), можно переходить к шагу «Создание проекта».
Если команда `dotnet` не найдена или версия ниже 9.0, выполните установку.

2. **Добавление репозитория Microsoft (пример для Ubuntu/Debian)**

   ```bash
   wget https://packages.microsoft.com/config/debian/12/packages-microsoft-prod.deb -O packages-microsoft-prod.deb
   sudo dpkg -i packages-microsoft-prod.deb
   rm packages-microsoft-prod.deb
   ```

3. **Установка .NET SDK 9.0**

   ```bash
   sudo apt-get update && \
     sudo apt-get install -y dotnet-sdk-9.0
   ```

4. **Проверка установки**

   ```bash
   dotnet --list-sdks
   ```

   В списке должна присутствовать версия типа `9.0.x`.

> Примечание: для других дистрибутивов (Fedora, Arch Linux и т. д.) инструкции по установке .NET SDK см. на официальном сайте Microsoft:
> [https://learn.microsoft.com/ru-ru/dotnet/core/install/linux](https://learn.microsoft.com/ru-ru/dotnet/core/install/linux)

---

## Создание и настройка проекта

1. **Перейдите в каталог, где будет располагаться проект**
   Например:

   ```bash
   mkdir ~/CalculatorFSharp
   cd ~/CalculatorFSharp
   ```

2. **Создайте новый консольный проект на F#**

   ```bash
   dotnet new console -lang "F#" -o CalculatorApp
   ```

   После этой команды появится папка `CalculatorApp` со стандартной структурой:

   ```
   CalculatorApp/
   ├── CalculatorApp.fsproj
   └── Program.fs
   ```

3. **Удалите или переименуйте стандартный файл `Program.fs`**
   Чтобы использовать ваш код из `CalculatorFuncProg.fs`, заменим его.

   ```bash
   cd CalculatorApp
   rm Program.fs
   ```

   Либо, если хотите сохранить оригинал, переименуйте:

   ```bash
   mv Program.fs _Program_backup.fs
   ```

---

## Добавление исходного файла

1. **Скопируйте ваш файл `CalculatorFuncProg.fs` в папку проекта**
   Предположим, файл лежит у вас в папке `~/Downloads`. Выполните:

   ```bash
   cp ~/Downloads/CalculatorFuncProg.fs .
   ```

   После этого в корне `CalculatorApp` появится:

   ```
   CalculatorFuncProg.fs
   CalculatorApp.fsproj
   ```

2. **Откройте `.fsproj` и убедитесь, что подключён `CalculatorFuncProg.fs`**
   Откройте файл `CalculatorApp.fsproj` в любимом текстовом редакторе, например:

   ```bash
   nano CalculatorApp.fsproj
   ```

   Структура файла по умолчанию будет такой:

   ```xml
   <Project Sdk="Microsoft.NET.Sdk">

     <PropertyGroup>
       <OutputType>Exe</OutputType>
       <TargetFramework>net9.0</TargetFramework>
     </PropertyGroup>

     <ItemGroup>
       <Compile Include="CalculatorFuncProg.fs" />
     </ItemGroup>

   </Project>
   ```

   Если `<Compile Include="Program.fs" />` всё ещё присутствует (после переименования/удаления стандартного файла), удалите его или замените на строку:

   ```xml
   <Compile Include="CalculatorFuncProg.fs" />
   ```

   Сохраните (`Ctrl+O`, Enter) и выйдите (`Ctrl+X`).

> **Важно:** Важно, чтобы в секции `<ItemGroup>` был указан именно ваш файл `CalculatorFuncProg.fs` и чтобы он подключался **первым** (F# компилятор требует, чтобы зависимости шли в правильном порядке). Если у вас в коде есть другие модули (`module …`) или зависимости, добавьте их в нужной последовательности.

---

## Сборка и запуск

1. **Сборка проекта**
   Находясь в папке `CalculatorApp`, выполните:

   ```bash
   dotnet build
   ```

   Вы должны увидеть что-то вроде:

   ```
   Microsoft (R) Build Engine версии 17.x.x+...
     Determining projects to restore...
     All projects are up-to-date for restore.
     CalculatorApp -> /home/user/CalculatorFSharp/CalculatorApp/bin/Debug/net9.0/CalculatorApp.dll
   ```

   Если сборка прошла без ошибок, продолжайте.

2. **Запуск из ката проекта**
   Можно сразу запустить:

   ```bash
   dotnet run
   ```

   Программа скомпилируется (если это ещё не сделано) и запустится. В консоли появится приглашение к вводу выражений.

3. **Запуск с уже собранного файла**
   Если вы хотите запустить скомпилированное приложение напрямую:

   ```bash
   cd bin/Debug/net9.0
   dotnet CalculatorApp.dll
   ```

   В этом случае `CalculatorApp.dll` — ваш исполняемый файл в формате .NET, он будет выполняться через `dotnet`.

4. **Переход в Release-режим (опционально)**
   Для оптимизированной сборки:

   ```bash
   dotnet build -c Release
   ```

   Далее запуск:

   ```bash
   cd bin/Release/net9.0
   dotnet CalculatorApp.dll
   ```

   В Release-версии обычно меньше проверок, больше оптимизаций, но чуть дольше компиляция.

---

## Примеры использования

1. **Вычисление простого выражения**

   ```
   > dotnet run
   ```

   (ждёт ввода)

   ```
   2 + 3 * 4
   ```

   Ответ:

   ```
   Результат: 14.000000
   ```

2. **Присваивание переменных**

   ```
   > let x = 5
   ```

   Вывод:

   ```
   Успешно: x = 5.000000
   ```

   ```
   > let y = x * 2 + 3
   ```

   Вывод:

   ```
   Успешно: y = 13.000000
   ```

   ```
   > x + y
   ```

   Вывод:

   ```
   Результат: 18.000000
   ```

3. **Символьное дифференцирование**

   ```
   > diff sin(x ^ 2) d x
   ```

   Вывод:

   ```
   Производная: 2.0 * x * cos(x ^ 2.0)
   ```

4. **Обработка ошибок**

   ```
   > (2 + 3
   ```

   Вывод:

   ```
   Ошибка: Несбалансированные скобки
   ```

   ```
   > 1 / 0
   ```

   Вывод:

   ```
   Ошибка: Деление на ноль
   ```



