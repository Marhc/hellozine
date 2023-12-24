# Hellozine
A collection of different ways to write "Hello World!".

### Echo on Bash

```bash
echo Hello World!
```

### Echo with parameter expansion on Bash

```bash
echo "Hello $(echo World)!"

name='World'
echo "Hello $name!"

message='Hello @!'
echo "${message/@/World}"
```

### Printf on Bash

```bash
printf 'Hello %s!\n' World
```

### Echo on Powershell

```powershell
echo 'Hello World!'
```

### Write-Host on Powershell

```powershell
Write-Host Hello World!
```

### Echo or Write-Host with interpolation on Powershell

```powershell
echo ('Hello {0}!' -f 'World')
Write-Host ('Hello {0}!' -f 'World')

$name = 'World'
echo "Hello $name!"
```

### Print on Python

```py
print('Hello World!')
```

### Print with string interpolation on Python

```python
print('Hello %s!' % 'World')
```

### Print with format method on Python

```python
print('Hello {0}!'.format('World'))
```

### Print with f-strings on Python

```python
print(f'Hello { "World" }!')
print(f"Hello { 'World' }!")
```

### Echo, print, printf and print_r on PHP

```php
<?php
  echo "Hello World!\n";
  print("Hello World!\n");
  printf("Hello World!\n");
  print_r("Hello World!\n");
```

### Printf and echo with string interpolation on PHP

```php
<?php
  printf("Hello %s!\n", "World");

  $name = 'World';
  echo "Hello $name!\n";
```

### Console.log on Javascript

```javascript
console.log('Hello World!');
```

### Console.log with string interpolation on Javascript

```javascript
console.log('Hello %s!', 'World');
```

### Console.log with Template literals on Javascript

```javascript
const message = 'World';
console.log(`Hello ${ message }!`);
```

### Alert on Javascript (Browser only)

```java
alert('Hello World!');
```

### Document.write on Javascript (Browser only)

```javascript
document.write("Hello World!");
```

### Before pseudo-element on CSS

```css
body::before {
  content: "Hello World!";
}
```

### Select on SQL

```sql
SELECT 'Hello World!';
```

### HTML

```html
<!DOCTYPE html>
<html>
<head>
  <title>Hello World!</title>
</head>
<body>
  <p>Hello World!</p>
</body>
</html>
```

### JSON

```json
{
  "message": "Hello World!"
}
```

### XML

```xml
<xml>
  <message>Hello World!</message>
</xml>
```

### YAML

```yml
---
message: Hello World!
```

### Markdown

```markdown
Hello World!


```

### TOML

```toml
[message]
text = "Hello World!"
```

### CSV

```csv
message
Hello World!
```

### jQuery

```html
<!DOCTYPE html>
<html>
<head>
  <script src="https://code.jquery.com/jquery-3.6.0.min.js"></script>
</head>
<body>
  <script>
    $(document).ready(function() {
      $("body").append("<p>Hello World!</p>");
    });
  </script>
</body>
</html>
```
