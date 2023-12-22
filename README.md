# hellozin
A collection of different ways to write "Hello World!".

### Echo on Bash

```bash
echo Hello World!
```

### Echo with interpolation on Bash

```bash
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

