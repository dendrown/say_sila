# Sentiment140/Kaggle datasets

Before any continued use of this data:
- Recreate all ARFFs (PRNG-seeded train/test sets) so that the ID is type String.
- Observe these functions in say.senti when making the ID changes:
  - init-instance
  - add-instance
  - create-arffs

We've recreated the base 100k-instance versions of these datasets with
String IDs, but the full-data versions will need to be done as well.
