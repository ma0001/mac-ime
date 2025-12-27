---
agent: 'agent'
description: 'コミットメッセージを生成する'
model: 'Gemini 3 Flash (Preview)'
tools: ['search', 'todo']
---

今回のセッション内容からコミットメッセージを英語で生成してください。

以下のルールに従ってください：
- 動詞の命令形で始めること（例："Add", "Fix", "Update"）。
- どう修正したかでなく、修正の目的を説明すること。
