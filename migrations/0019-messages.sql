-- 0019: addressing (todo/c3208d48) - the public message log.
--
-- A message is an append-only row.  The sender is the identity the
-- posting KEY resolves to, never a request field (a payload carrying
-- "from" is refused), so spoofing is a shape that does not exist
-- rather than a check that might be forgotten.  Reads are public:
-- the log is a filtered view, not a security boundary, and the same
-- rule as the board applies (no secrets ride a message).
--
-- The inbox cursor is server-side (message_cursors): a consumer that
-- dies and restarts neither replays nor drops a row, and the status
-- endpoint answers while the consumer is dead because it only reads
-- the cursor row.

CREATE TABLE IF NOT EXISTS messages (
  seq bigserial PRIMARY KEY,
  sender text NOT NULL,
  recipient text NOT NULL,
  kind text NOT NULL DEFAULT 'note',
  re text NULL,                        -- free-form reference (a seq, a card id)
  body text NOT NULL,
  body_hash text NOT NULL,             -- sha256 hex of body
  created_at timestamptz NOT NULL DEFAULT now());

CREATE INDEX IF NOT EXISTS messages_recipient_seq ON messages (recipient, seq);

CREATE TABLE IF NOT EXISTS message_cursors (
  inbox text PRIMARY KEY,
  last_seq bigint NOT NULL DEFAULT 0,
  last_poll_at timestamptz NULL);

INSERT INTO schema_migrations (name) VALUES ('0019-messages.sql')
  ON CONFLICT (name) DO NOTHING;
