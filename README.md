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

### CSS

```css
body::before {
  content: "Hello World!";
}
```

### SQL

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
  <title>Hello World!</title>
</head>
<body>
  <script src="https://code.jquery.com/jquery-3.6.0.min.js"></script>
  <script>
    $(document).ready(function() {
      $("body").append("<p>Hello World!</p>");
    });
  </script>
</body>
</html>
```

### SASS

```sass
@import 'compass/reset'

$primary-color: #ff0000

body
  background-color: $primary-color
  color: white
  text-align: center
  font-size: 24px
  padding-top: 100px

  &:before
    content: 'Hello World!'
    display: block
```

### SCSS

```scss
$greeting: "Hello World!";

body:before {
  content: $greeting;
  display: block;
  text-align: center;
  font-size: 24px;
  font-weight: bold;
  padding: 20px;
  background-color: #f7f7f7;
  border: 1px solid #ccc;
  margin: 20px auto;
  width: 200px;
}
```

### Java

```java
public class HelloWorld {
    public static void main(String[] args) {
        System.out.println("Hello World!");
    }
}
```

### R

```r
cat("Hello World!")
```

### Cobol

```cobol
        IDENTIFICATION DIVISION.
        PROGRAM-ID. HELLOWORLD.
        PROCEDURE DIVISION.
          DISPLAY 'Hello World!'.
        STOP RUN.
```

### VBScript

```vbscript
MsgBox "Hello World!"
```

### VB.NET

```vb
Module HelloWorld
    Sub Main()
        Console.WriteLine("Hello World!")
    End Sub
End Module
```

### VBA

```vb
Sub HelloWorld()
    MsgBox "Hello World!"
End Sub
```

### Swift

```swift
print("Hello World!")
```

### Kotlin

```kotlin
fun main() {
    println("Hello World!")
}
```

### Dart

```dart
void main() {
  print("Hello World!");
}
```

### Express.js

```javascript
const express = require('express');
const app = express();
const port = 3000;

app.get('/', (_, res) => {
  res.send('Hello World!');
});

app.listen(port, () => {
  console.log(`Server listening on port ${port}`);
});
```

### Flask

```python
from flask import Flask

app = Flask(__name__)

@app.route('/')
def hello_world():
    return 'Hello World!'

if __name__ == '__main__':
    app.run()
```

### FastAPI

```python
from fastapi import FastAPI

app = FastAPI()

@app.get("/")
def hello_world():
    return "Hello World!"
```

### Django

```python
from django.http import HttpResponse
from django.urls import path
from django.conf import settings
from django.conf.urls.static import static
from django.core.wsgi import get_wsgi_application
from django.core.management import execute_from_command_line
from django.core.exceptions import ImproperlyConfigured
from django.core.validators import validate_email

class HelloWorldView:
    def __init__(self):
        self.response = HttpResponse("Hello World!")

    def __call__(self, request):
        return self.response

urlpatterns = [
    path('', HelloWorldView()),
] + static(settings.STATIC_URL, document_root=settings.STATIC_ROOT)

application = get_wsgi_application()

if __name__ == '__main__':
    try:
        validate_email(settings.DEFAULT_FROM_EMAIL)
        execute_from_command_line()
    except ImproperlyConfigured as e:
        print(f"Error: {e}")
```

### Laravel

```php
Route::get('/', function () {
    return 'Hello World!';
});
```

### .NET Core Web API

```csharp
using Microsoft.AspNetCore.Mvc;

namespace HelloWorld.Controllers
{
    [ApiController]
    [Route("[controller]")]
    public class HelloWorldController : ControllerBase
    {
        [HttpGet]
        public ActionResult<string> Get()
        {
            return "Hello World!";
        }
    }
}
```

### C#

```csharp
using System;

class Program
{
    static void Main(string[] args)
    {
        Console.WriteLine("Hello World!");
    }
}
```

### String interpolation on C#

```csharp
using System;

class Program
{
  static void Main(string[] args)
  {
    String name = "World";
    Console.WriteLine($"Hello { name }!");
    Console.WriteLine("Hello {0}!", "World");
  }
}
```

### Dockerfile

```dockerfile
FROM alpine:latest

CMD ["echo", "Hello World!"]
```

### Docker CLI

```shell
docker run hello-world
docker run --rm alpine:latest echo Hello World!
```

### Functional Component on React

```jsx
import React from 'react';

function HelloWorld() {
  return <h1>Hello World!</h1>;
}

export default HelloWorld;
```

### Class Component on React

```jsx
import React from 'react';

class HelloWorld extends React.Component {
  render() {
    return (
      <div>
        <h1>Hello World!</h1>
      </div>
    );
  }
}

export default HelloWorld;
```

### Vue

```vue
<template>
  <div>
    <h1>{{ greeting }}</h1>
  </div>
</template>

<script>
export default {
  data() {
    return {
      greeting: 'Hello World!'
    };
  }
};
</script>
```

### Svelte

```svelte
<script>
  let greeting = 'Hello World!';
</script>

<main>
  <h1>{greeting}</h1>
</main>
```

### Angular

```ts
import { Component } from '@angular/core';

@Component({
  selector: 'app-hello-world',
  template: '<h1>{{ greeting }}</h1>'
})
export class HelloWorldComponent {
  greeting: string = 'Hello World!';
}
```

### Preact

```jsx
import { h, render } from 'preact';

const App = () => <h1>Hello World!</h1>;

render(<App />, document.body);
```

### Next

```jsx
export default function Home() {
  return (
    <div>
      <h1>Hello World!</h1>
    </div>
  );
}
```

### Micronaut

```java
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import io.micronaut.http.annotation.Produces;

@Controller("/hello")
public class HelloWorldController {

    @Get("/")
    @Produces(MediaType.TEXT_PLAIN)
    public String hello() {
        return "Hello World!";
    }
}
```

### RESTEasy

```java
@Path("/")
public class HelloWorldResource {

    @GET
    @Produces(MediaType.TEXT_PLAIN)
    public String sayHello() {
        return "Hello World!";
    }
}
```

### Spring Boot

```java
@RestController
public class HelloWorldController {

    @GetMapping("/hello")
    public String hello() {
        return "Hello World!";
    }
}
```

### TailwindCSS

```html
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <link href="https://cdn.jsdelivr.net/npm/tailwindcss@2.2.19/dist/tailwind.min.css" rel="stylesheet">
  <title>Hello World!</title>
</head>
<body>
  <div class="flex justify-center items-center h-screen">
    <h1 class="text-4xl text-blue-500 font-bold">Hello World!</h1>
  </div>
</body>
</html>
```

### Styled-Components

```jsx
import React from 'react';
import styled from 'styled-components';

const Greeting = styled.h1`
  color: #ff00ff;
  font-size: 24px;
  font-weight: bold;
`;

const HelloWorld = () => {
  return <Greeting>Hello World!</Greeting>;
};

export default HelloWorld;
```

### Bootstrap

```html
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css">
  <title>Hello World!</title>
</head>
<body>
  <div class="container">
    <h1>Hello World!</h1>
  </div>
</body>
</html>
```

### LESS (Leaner Style Sheets)

```less
@message: "Hello World!";
@color: red;

.hello-world {
  color: @color;
  content: @message;
}
```

### Emotion CSS

```jsx
import React from 'react';
import { css } from '@emotion/react';

const HelloWorld = () => {
  const helloWorldStyles = css`
    text-align: center;
    font-size: 24px;
    color: #333;
  `;

  return (
    <div css={helloWorldStyles}>
      Hello World!
    </div>
  );
};

export default HelloWorld;
```

### Stylus CSS

```stylus
hello-world
  text-align center
  font-size 24px
  color #333

div
  .hello-world
    Hello World!
```

### Lit

```js
import { html, css, LitElement } from 'lit';

class HelloWorld extends LitElement {
  static styles = css`
    :host {
      display: block;
      text-align: center;
      font-size: 24px;
      color: #333;
    }
  `;

  render() {
    return html`
      <div>
        Hello World!
      </div>
    `;
  }
}

customElements.define('hello-world', HelloWorld);
```

### JSS ("Javascript Style Sheets" or "CSS-in-JS")

```jsx
import React from 'react';
import ReactDOM from 'react-dom';

const styles = {
  helloWorld: {
    fontSize: '20px',
    color: 'red',
  },
};

const HelloWorld = () => {
  return (
    <div className={styles.helloWorld}>
      Hello World!
    </div>
  );
};

ReactDOM.render(<HelloWorld />, document.getElementById('root'));
```
