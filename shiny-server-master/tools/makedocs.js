#!/usr/bin/env node

/*
 * makedocs.js
 *
 * Copyright (C) 2009-13 by RStudio, Inc.
 *
 * This program is licensed to you under the terms of version 3 of the
 * GNU Affero General Public License. This program is distributed WITHOUT
 * ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
 * AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) for more details.
 *
 */

/*
 * This script is for generating config.html. It is designed to run under
 * node-supervisor (https://github.com/isaacs/node-supervisor).
 *
 *   supervisor -n exit --extensions 'js|html' lib/makedocs.js
 */

var fs = require('fs');
var path = require('path');
var util = require('util');
var htmlEscape = require('connect/lib/utils').escape;
var Handlebars = require('handlebars');
var _ = require('underscore');
var map = require('../lib/core/map');
var config = require('../lib/config/config');
var schema = require('../lib/config/schema');

function filterDesc(desc) {
  if (!desc)
    return desc;

  var seen = {};
  desc = desc.replace(/`(.*?)`/g, function(str, m) {
    if (rulesByName[m] && !seen[m]) {
      seen[m] = true;
      return '<a class="code" href="#' + m + '">' + m + '</a>';
    }
    else
      return '<code>' + m + '</code>';
  });

  desc = desc.replace(/\[(.*?)\]\((.*?)\)/g, function(str, m1, m2) {
    return '<a href="' + htmlEscape(m2) + '">' + htmlEscape(m1) + '</a>';
  });

  return desc;

  //'<code>$1</code>');
}

function scopelist(scopes) {
  if (typeof scopes == 'string')
    scopes = [scopes];
  return _.map(scopes, function(scope) {
    if (scope === '$')
      return 'Top-level';
    else
      return '<code><a href="#' + scope + '">' + scope + '</a></code>';
  }).join(', ');
}
Handlebars.registerHelper('scopelist', scopelist);
Handlebars.registerHelper('yesno', function(x) {
  return x ? 'yes' : 'no';
});
Handlebars.registerHelper('YesNo', function(x) {
  return x ? 'Yes' : 'No';
});


var packageInfo =
  JSON.parse(fs.readFileSync(path.join(__dirname, '../package.json')));
var version = packageInfo['version'];

var rulesPath = path.join(__dirname, '../lib/router/shiny-server-rules.config');
var ruleConfig = config.parse(fs.readFileSync(rulesPath, 'utf-8'), rulesPath);

var rules = _.map(ruleConfig.children, function(child) {
  var name = child.name;
  var desc = child.getOne('desc').args[0];
  var params = _.map(child.getAll('param'), function(param) {
    return new schema.ConfigSchemaParam(param);
  });
  var at = child.getOne('at').args;
  var primaryLoc = _.last(at);
  var otherLocs = _.clone(at);
  otherLocs.pop();

  return {
    name: name,
    version: packageInfo['version'],
    desc: desc,
    params: params,
    at: at,
    primaryLoc: primaryLoc,
    otherLocs: otherLocs,
    children: [],
    inheritedChildren: []
  }
});

var rulesByName = map.create();
_.each(rules, function(rule) {
  rulesByName[rule.name] = rule;
});
_.each(rules, function(rule) {
  if (rule.primaryLoc != '$')
    rulesByName[rule.primaryLoc].children.push(rule.name);
  _.each(rule.otherLocs || [], function(parent) {
    if (parent != '$')
      rulesByName[parent].inheritedChildren.push(rule.name);
  });
  rule.desc = filterDesc(rule.desc);
  _.each(rule.params, function(param) {
    param.desc = filterDesc(param.desc);
  });
});

var template = Handlebars.compile(fs.readFileSync(path.join(__dirname, '../templates/config.html'), 'utf-8'));
fs.writeFileSync(
  path.join(__dirname, '../config.html'),
  template({
    version: version,
    directives: rules,
    warning: "<!-- DO NOT EDIT BY HAND; automatically generated by makedocs.js -->"
  }),
  'utf-8'
);
