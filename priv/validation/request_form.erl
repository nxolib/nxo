{email, [required, email,
         {nodup, nxo_users, email, user_id, user_id}]}.

{first_name, [required]}.

{last_name, [required]}.

{phone, [phone]}.

{password, [required, {min, 12}]}.

{password_confirm, [required, {match, password}]}.
