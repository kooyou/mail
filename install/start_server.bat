%~d0
cd %~dp0
cd ../ebin
erl -boot start_sasl -config elog -s mail_app start -extra localhost root 1234
pause
