import sys, termios, tty, select

fd = sys.stdin.fileno()
old_settings = termios.tcgetattr(fd)
tty.setcbreak(fd)   # raw-ish mode, returns on every keypress

try:
    while True:
        r, _, _ = select.select([sys.stdin], [], [], 0.05)  # 50ms poll
        if r:
            ch = sys.stdin.read(1)
            sys.stdout.write(ch)
            sys.stdout.flush()
finally:
    termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
