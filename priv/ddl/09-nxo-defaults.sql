SELECT
  group_add('administrators', 'Global Administrators',
            'Users with all-access authorizations.',
            true),

  group_add('usermgmt', 'User Managers',
            'Users with permission to create and configure other users.',
            false),

  group_add('datamgmt', 'Data Managers',
            'Users with permission to edit data.',
            false),

  group_add('passwd', 'Stale Password',
            'Users requiring a password change on next login.',
            false),

  group_add('api', 'API Users',
            'Users with API access.',
            false),

  group_add('pending', 'Pending Users',
            'Users who are awaiting login access.',
            false);


SELECT
  org_add('Global Organization', 'global'),
  org_add('Ace Corporation', 'ace'),
  org_add('Acme Co.', 'acme');

SELECT
  user_add('quux@example.com',
           '$2a$12$SRBNjPnX167CP/J9t7.S/ep.XZMmfmFTuRKGBOmIRpcbVmWN0vV5O',
           '555-1212', 'Seed', 'User', true, 'Seed Account');

SELECT
  add_user_org(user_id('quux@example.com'), org_id('global')),
  add_user_group(user_id('quux@example.com'),
                 group_id('administrators'),
                 org_id('global'));
