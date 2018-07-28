[![CircleCI](https://circleci.com/gh/waddlaw/yesod-form-bulma/tree/master.svg?style=svg)](https://circleci.com/gh/waddlaw/yesod-form-bulma/tree/master)
[![Hackage](https://img.shields.io/hackage/v/yesod-form-bulma.svg)](https://hackage.haskell.org/package/yesod-form-bulma)

# yesod-form-bulma

- [Bulma](https://bulma.io/documentation/form/)
- [Bulma-Extensions](https://wikiki.github.io/form/checkradio/)

## Screenshot

![a](https://i.imgur.com/SZnv42b.png)

## Examples

simple form example.

```sh
$ stack build
$ stack exec form-example
```

full components showcase.

```sh
$ stack build
$ stack exec form-showcase
```

Access to [localhost:3100](http://localhost:3100)

## Implemented status

### Yesod.Form.Fields

#### Fields

- [x] bulmaTextField
- [ ] bulmaPasswordField
- [x] bulmaTextareaField
- [ ] bulmaHiddenField
- [x] bulmaIntField
- [ ] bulmaDayField
- [ ] bulmaTimeFieldTypeTime
- [ ] bulmaTimeFieldTypeText
- [ ] bulmaHtmlField
- [x] bulmaEmailField
- [ ] bulmaMultiEmailField
- [ ] bulmaSearchField
- [ ] bulmaUrlField
- [ ] bulmaDoubleField
- [ ] bulmaBoolField
- [x] bulmaCheckBoxField
- [ ] bulmaFileField

#### Options

- [x] bulmaSelectField
- [x] bulmaSelectFieldList
- [x] bulmaRadioField
- [x] bulmaRadioFieldList
- [x] bulmaCheckboxesField
- [x] bulmaCheckboxesFieldList
- [x] bulmaMultiSelectField
- [x] bulmaMultiSelectFieldList
