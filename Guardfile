notification :tmux, display_message: true

# Runs the command and prints a notification
def execute(subject, pass_msg, fail_msg, cmd)
  if system(cmd)
    n pass_msg, subject, :success
  else
    n fail_msg, subject, :failed
  end
end

def run_all_tests
  execute 'Hspec', 'Test success!', 'Test failure!', %{
    ./run-tests.sh
  }
end

guard :shell, all_after_pass: false do
  watch(%r{^src/(.+)\.hs$})       { run_all_tests }
  watch(%r{^test/.+\.hs})             { run_all_tests }
end

