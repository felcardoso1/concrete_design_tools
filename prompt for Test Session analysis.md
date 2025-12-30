This repository is a suite of cli tools made for agentic AI use, as you can read in @CLAUDE.md 
The suite of tools is functional and in production.
For testing purposes, we made tests to evalute the use of the tools by AI agents in a scenario of actual normal daily use. All the relevant files for this are in the @Test sessions  folder.
For that, we created a standard prompt submitted for the agent use, which is the file @test_sandbox/prompt for opencode.md 
After running this prompt, we asked the AI agent a few questions about the tools he just used, the prompt used is at @test_sandbox/prompt for interview.md 

We ran this test through four different AI agents with different models. For each test there's a folder inside the Test Sessions folder, and in each of these folders there's the  terminal output in a .txt file, and a .md file which is the file with the answer given by the AI to the questionaire about the tools he just used.

Your task is to analyze these test sessions and provide a summary on them. Also, provide a summary of  the discrepance between the "impressions" by the AI agent and an analysis of its actual terminal output. Sometimes the tool might say something "nice" and hide the dificulties it faced, that cant be hidden in the actual terminal output.
You should also analyze  whether the "feedback" written by the AI agent isnt just an halucination or something he read in the documentation instead of his actual experience. We must be careful to not contaminte the results with halucination from the LLM.

When you are finished, write a .md file with a report on your Findings and analysis.
Also create  another .md file with suggestions of improvements for the cli tools.

Currently you must NOT edit any of the actual tool files, you must only analyze the test sessions as instructed, and create the two report files I mentioned. 