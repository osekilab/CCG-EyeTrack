# Data for the main analysis
Both of corpora we used in this study, the Dundee corpus and [BCCWJ-EyeTrack](https://clrd.ninjal.ac.jp/bccwj/en/index.html) are licensed.


The predictors were obtained by the following tools or data:
- Dependency
    - English: [The Dundee Treebank](https://bitbucket.org/lowlands/release/src/master/TLT2015/)
    - Japanese: [BCCWJ-DepPara](https://clrd.ninjal.ac.jp/bccwj/en/index.html)
- Frequency
    - English: [Wikipedia word frequency generator](https://github.com/IlyaSemenov/wikipedia-word-frequency)
    - Japanese: [N-gram frequency table of NINJAL Web Corpus](https://www.gsk.or.jp/catalog/gsk2020-c/)
- CCG parser
    - English: [ccgtools](https://github.com/stanojevic/ccgtools)
    - Japanese: [depccg](https://github.com/masashi-y/depccg)
    - The best parses for each sentence are in `./parse/`
        - The original text (terminal symbols) was masked with "_" due to the copyright issue.
- GPT-2
    - English: [OpenAI model](https://huggingface.co/openai-community/gpt2)
    - Japanese: [rinna model](https://huggingface.co/rinna/japanese-gpt2-medium)

