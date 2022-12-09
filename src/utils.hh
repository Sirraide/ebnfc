#ifndef EBNFC_UTILS_HH
#define EBNFC_UTILS_HH

#include <fmt/format.h>

using u8 = uint8_t;
using u16 = uint16_t;
using u32 = uint32_t;
using u64 = uint64_t;
using i8 = int8_t;
using i16 = int16_t;
using i32 = int32_t;
using i64 = int64_t;
using f32 = float;
using f64 = double;
using usz = size_t;
using isz = ptrdiff_t;

#define CAT_(x, y) x##y
#define CAT(x, y) CAT_(x, y)

#define STR_(x) #x
#define STR(x) STR_(x)

#define defer auto CAT($$defer_instance_, __COUNTER__) = $$defer{}, [&]()
#define tempset $$tempset_type CAT($$tempset_instance_, __COUNTER__) = $$tempset_stage_1{} %

template <typename callable>
struct $$defer_type {
    callable cb;
    explicit $$defer_type(callable&& _cb) : cb(std::forward<callable>(_cb)) {}
    ~$$defer_type() { cb(); }
};

struct $$defer {
    template <typename callable>
    $$defer_type<callable> operator,(callable&& cb) {
        return $$defer_type<callable>{std::forward<callable>(cb)};
    }
};

template <typename type>
struct $$tempset_type {
    type& ref;
    type t;
    type oldval;

    explicit $$tempset_type(type& var, std::convertible_to<type> auto&& cv) : ref(var), t(std::forward<decltype(cv)>(cv)) {
        oldval = std::move(ref);
        ref = std::move(t);
    }

    ~$$tempset_type() { ref = std::move(oldval); }
};

template <typename type>
struct $$tempset_stage_2 {
    type& ref;

    $$tempset_stage_2(type& var) : ref(var) {}
    $$tempset_type<type> operator=(std::convertible_to<type> auto&& value) {
        return $$tempset_type<type>{ref, std::forward<decltype(value)>(value)};
    }
};

struct $$tempset_stage_1 {
    template <typename type>
    $$tempset_stage_2<type> operator%(type& var) {
        return $$tempset_stage_2<type>{var};
    }
};

namespace ebnfc {

template <typename ...arguments>
void die(fmt::format_string<arguments...> fmt, arguments&& ...args) {
    fmt::print(stderr, "Error: ");
    fmt::print(stderr, fmt, std::forward<arguments>(args)...);
    fmt::print(stderr, "\n");
    exit(1);
}

template <typename ...arguments>
void todo(fmt::format_string<arguments...> fmt = "", arguments&& ...args) {
    fmt::print(stderr, "TODO: ");
    if (fmt.get().size() != 0) fmt::print(stderr, fmt, std::forward<arguments>(args)...);
    else fmt::print (stderr, "Unimplemented");
    fmt::print(stderr, "\n");
    exit(42);
}

template <typename a, typename b>
constexpr inline bool is = std::is_same_v<std::remove_cvref_t<a>, std::remove_cvref_t<b>>;

}

#endif // EBNFC_UTILS_HH
