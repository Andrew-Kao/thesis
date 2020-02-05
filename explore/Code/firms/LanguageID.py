import pdb

from google.cloud import translate_v2 as translate
translate_client = translate.Client()

text = "abc"

# Text can also be a sequence of strings, in which case this method
# will return a sequence of results for each text.
result = translate_client.detect_language(text)

print('Text: {}'.format(text))
print('Confidence: {}'.format(result['confidence']))
print('Language: {}'.format(result['language']))
pdb.set_trace()