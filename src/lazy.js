function lazy(thunk) {
    return {
        force: function() {
            var value = thunk();

            this.force = function() {
                return value;
            };

            return value;
        }
    };
}

function force(l) {
    return l.force();
}
