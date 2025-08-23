/**
 * @fileoverview Enforce <marquee> elements are not used.
 * @author Ethan Cohen
 */

// -----------------------------------------------------------------------------
// Requirements
// -----------------------------------------------------------------------------

import { RuleTester } from 'eslint';
import parserOptionsMapper from '../../__util__/parserOptionsMapper';
import rule from '../../../src/rules/accessible-emoji';
import parsers from '../../__util__/helpers/parsers';

// -----------------------------------------------------------------------------
// Tests
// -----------------------------------------------------------------------------

const ruleTester = new RuleTester();

const expectedError = {
  message: 'Emojis should be wrapped in <span>, have role="img", and have an accessible description with aria-label or aria-labelledby.',
  type: 'JSXOpeningElement',
};

ruleTester.run('accessible-emoji', rule, {
  valid: parsers.all([].concat(
    { code: '<div />;' },
    { code: '<span />' },
    { code: '<span>No emoji here!</span>' },
    { code: '<span role="img" aria-label="Panda face">ðŸ¼</span>' },
    { code: '<span role="img" aria-label="Snowman">&#9731;</span>' },
    { code: '<span role="img" aria-labelledby="id1">ðŸ¼</span>' },
    { code: '<span role="img" aria-labelledby="id1">&#9731;</span>' },
    { code: '<span role="img" aria-labelledby="id1" aria-label="Snowman">&#9731;</span>' },
    { code: '<span>{props.emoji}</span>' },
    { code: '<span aria-hidden>{props.emoji}</span>' },
    { code: '<span aria-hidden="true">ðŸ¼</span>' },
    { code: '<span aria-hidden>ðŸ¼</span>' },
    { code: '<div aria-hidden="true">ðŸ¼</div>' },
    { code: '<input type="hidden">ðŸ¼</input>' },
    {
      code: '<CustomInput type="hidden">ðŸ¼</CustomInput>',
      settings: { 'jsx-a11y': { components: { CustomInput: 'input' } } },
    },
    {
      code: '<Box as="input" type="hidden">ðŸ¼</Box>',
      settings: { 'jsx-a11y': { polymorphicPropName: 'as' } },
    },
  )).map(parserOptionsMapper),
  invalid: parsers.all([].concat(
    { code: '<span>ðŸ¼</span>', errors: [expectedError] },
    { code: '<span>fooðŸ¼bar</span>', errors: [expectedError] },
    { code: '<span>foo ðŸ¼ bar</span>', errors: [expectedError] },
    { code: '<i role="img" aria-label="Panda face">ðŸ¼</i>', errors: [expectedError] },
    { code: '<i role="img" aria-labelledby="id1">ðŸ¼</i>', errors: [expectedError] },
    { code: '<Foo>ðŸ¼</Foo>', errors: [expectedError] },
    { code: '<span aria-hidden="false">ðŸ¼</span>', errors: [expectedError] },
    { code: '<CustomInput type="hidden">ðŸ¼</CustomInput>', errors: [expectedError] },
    {
      code: '<Box as="span">ðŸ¼</Box>',
      settings: { 'jsx-a11y': { polymorphicPropName: 'as' } },
      errors: [expectedError],
    },
  )).map(parserOptionsMapper),
});

