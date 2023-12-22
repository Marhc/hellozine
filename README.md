# hellozin
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

