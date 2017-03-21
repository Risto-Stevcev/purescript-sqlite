"use strict";

// module Sqlite.Core

var sqlite3 = require('sqlite3');

var sqlParamsToObj = function(params) {
  return params.reduce(function(obj, param) {
    obj[param.value0] = param.value1.value0;
    return obj;
  }, {});
}

exports._setVerbose = function() {
  sqlite3.verbose()
}

exports._OPEN_READONLY = sqlite3.OPEN_READONLY;
exports._OPEN_READWRITE = sqlite3.OPEN_READWRITE;
exports._OPEN_CREATE = sqlite3.OPEN_CREATE;

exports._connect = function(filename, mode, cached, errback, callback) {
  return function() {
    var Database = cached ? sqlite3.cached.Database : sqlite3.Database;

    var db = new Database(filename, mode, function(err) {
      if (err) return errback(err)();
      callback(db)();
    });
  }
}

exports._close = function(db, errback, callback) {
  return function() {
    db.close(function(err) {
      if (err) return errback(err)();
      callback();
    });
  }
}

exports._run = function(db, query, errback, callback) {
  return function() {
    db.run(query, function(err) {
      if (err) return errback(err)();

      var lastID = this.lastID; // Only for INSERT
      var changes = this.changes; // Only for UPDATE or DELETE
      callback({ lastID: lastID, changes: changes })();
    });
  }
}

exports._getOne = function(db, query, errback, callback) {
  return function() {
    db.get(query, function(err, row) {
      if (err) return errback(err)();
      callback(row === undefined ? null : row)();
    });
  }
}

exports._get = function(db, query, errback, callback) {
  return function() {
    db.all(query, function(err, rows) {
      if (err) return errback(err)();
      callback(rows)();
    });
  }
}


exports._stmtPrepare = function(db, query, errback, callback) {
  return function() {
    var statement = db.prepare(query, function(err) {
      if (err) return errback(err)();
      callback(statement)();
    });
  }
}

exports._stmtBind = function(stmt, params, errback, callback) {
  return function() {
    stmt.bind(params, function(err) {
      if (err) return errback(err)();
      callback();
    });
  }
}

exports._stmtReset = function(stmt, callback) {
  return function() {
    stmt.reset(callback);
  }
}

exports._stmtFinalize = function(stmt, callback) {
  return function() {
    stmt.finalize(callback);
  }
}

exports._stmtRun = function(stmt, params, errback, callback) {
  return function() {
    stmt.run(sqlParamsToObj(params), function(err) {
      if (err) return errback(err)();
      var lastID = this.lastID; // Only for INSERT
      var changes = this.changes; // Only for UPDATE or DELETE
      callback({ lastID: lastID, changes: changes })();
    });
  }
}

exports._stmtGetOne = function(stmt, params, errback, callback) {
  return function() {
    stmt.get(params, function(err, row) {
      if (err) return errback(err)();
      callback(row === undefined ? null : row)();
    });
  }
}

exports._stmtGet = function(stmt, params, errback, callback) {
  return function() {
    stmt.all(params, function(err, rows) {
      if (err) return errback(err)();
      callback(rows)();
    });
  }
}


exports._dbListener = function(fn) {
  return function(result) {
    return fn(result)();
  };
};

exports._dbListenerFn2 = function(fn) {
  return function(result, result2) {
    return fn(result, result2)();
  };
};

exports._listen = function(db, eventType, callback) {
  return function() {
    db.on(eventType, callback);
    return {};
  }
}
